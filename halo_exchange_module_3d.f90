Module halo_exchange_3d_module

  Use numbers_module, Only : wp

  Implicit None
  
  Type, Private :: buffer
     Integer                                       :: remote_rank
     Integer   , Dimension( 1:3     )              :: dir
     Integer   , Dimension( 1:3     )              :: ilo
     Integer   , Dimension( 1:3     )              :: ihi
     Real( wp ), Dimension( :, :, : ), Allocatable :: buffer
     Integer                                       :: request
  End type buffer

  Type, Public :: halo_exchange
     Integer                                    , Private :: communicator
     Type( buffer ), Dimension( : ), Allocatable, Private :: send_buffers
     Type( buffer ), Dimension( : ), Allocatable, Private :: recv_buffers
   Contains
     Procedure, Public :: init
     Procedure, Public :: exchange
  End type halo_exchange

  Private

Contains

  Subroutine init( buffy, ng, width, communicator, proc_map )

    Implicit None

    ! Note assumes need to send in  all directions currently
    ! Ultimately there is an optimisation for orthogonal
    ! pairs of vectors that cuts down on the amount of data that
    ! needs to be sent
    
    Class( halo_exchange )                    , Intent(   Out ) :: buffy
    Integer,  Dimension( 1:3                 ), Intent( In    ) :: ng
    Integer                                   , Intent( In    ) :: width
    Integer                                   , Intent( In    ) :: communicator
    Integer,  Dimension( -1:+1, -1:+1, -1:+1 ), Intent( In    ) :: proc_map

    Integer :: i1, i2, i3
    Integer :: icomm
    
    buffy%communicator = communicator

    Allocate( buffy%send_buffers( 1:26 ) )
    Allocate( buffy%recv_buffers( 1:26 ) )

    icomm = 0
    Do i1 = -1, +1
       Do i2 = -1, +1
          Do i3 = -1, +1
             If( All( [ i1, i2, i3 ] == 0 ) ) Cycle ! No comms to self
             icomm = icomm + 1
             ! Send buffer - data in our domain to be sent to the remote process into its halo
             buffy%send_buffers( icomm )%remote_rank = proc_map( i1, i2, i3 )
             buffy%send_buffers( icomm )%dir         = [ i1, i2, i3 ]
             buffy%send_buffers( icomm )%ilo         = ilo_send_calc( [ i1, i2, i3 ], ng, width )
             buffy%send_buffers( icomm )%ihi         = ihi_send_calc( [ i1, i2, i3 ], ng, width )
             Associate( ilo => buffy%send_buffers( icomm )%ilo, ihi => buffy%send_buffers( icomm )%ihi )
               Allocate( buffy%send_buffers( icomm )%buffer( ilo( 1 ):ihi( 1 ), ilo( 2 ):ihi( 2 ), ilo( 3 ):ihi( 3 ) ) )
             End Associate
             ! Recv buffer - data in the remote domain to be put into this processes' halo
             buffy%recv_buffers( icomm )%remote_rank = proc_map( i1, i2, i3 )
             buffy%recv_buffers( icomm )%dir         = [ i1, i2, i3 ]
             buffy%recv_buffers( icomm )%ilo         = ilo_recv_calc( [ i1, i2, i3 ], ng, width )
             buffy%recv_buffers( icomm )%ihi         = ihi_recv_calc( [ i1, i2, i3 ], ng, width )
             Associate( ilo => buffy%recv_buffers( icomm )%ilo, ihi => buffy%recv_buffers( icomm )%ihi )
               Allocate( buffy%recv_buffers( icomm )%buffer( ilo( 1 ):ihi( 1 ), ilo( 2 ):ihi( 2 ), ilo( 3 ):ihi( 3 ) ) )
             End Associate
          End Do
       End Do
    End Do

  Contains

    Elemental Function ilo_send_calc( dir, n, w ) Result( ilo )

      Integer :: ilo
      
      Integer, Intent( In ) :: dir
      Integer, Intent( In ) :: n
      Integer, Intent( In ) :: w

      If( dir /= 1 ) Then
         ilo = 0
      Else
         ilo = n - w
      End If

    End Function ilo_send_calc
    
    Elemental Function ihi_send_calc( dir, n, w ) Result( ihi )

      Integer :: ihi
      
      Integer, Intent( In ) :: dir
      Integer, Intent( In ) :: n
      Integer, Intent( In ) :: w

      If( dir == - 1 ) Then
         ihi = w - 1
      Else
         ihi = n - 1
      End If

    End Function ihi_send_calc
    
    Elemental Function ilo_recv_calc( dir, n, w ) Result( ilo )

      Integer :: ilo
      
      Integer, Intent( In ) :: dir
      Integer, Intent( In ) :: n
      Integer, Intent( In ) :: w

      ilo = Huge( ilo )
      Select Case( dir )
      Case( -1 )
         ilo = -w
      Case( 0 )
         ilo = 0
      Case( +1 )
         ilo = n
      End Select

    End Function ilo_recv_calc
    
    Elemental Function ihi_recv_calc( dir, n, w ) Result( ihi )

      Integer :: ihi
      
      Integer, Intent( In ) :: dir
      Integer, Intent( In ) :: n
      Integer, Intent( In ) :: w

      ihi = Huge( ihi )
      Select Case( dir )
      Case( -1 )
         ihi = -1
      Case( 0 )
         ihi = n - 1
      Case( +1 )
         ihi = n + w - 1
      End Select

    End Function ihi_recv_calc
    
  End Subroutine init

  Subroutine exchange( buffy, lb, data )

    ! Note assumes 0: contains the data for this domain, so negative indices and
    ! above the data for the domain are the halos. May want to think about a cleaner
    ! interface to this

    Use mpi, Only : MPI_Type_create_f90_real, MPI_Irecv, MPI_Isend, MPI_Waitall, MPI_Waitany, &
         MPI_UNDEFINED, MPI_STATUS_IGNORE, MPI_STATUSES_IGNORE, MPI_Comm_rank

    Class( halo_exchange )                               , Intent( InOut ) :: buffy
    Integer   , Dimension( 1:3 )                         , Intent( In    ) :: lb
    Real( wp ), Dimension( lb( 1 ):, lb( 2 ):, lb( 3 ): ), Intent( InOut ) :: data

    Integer :: datatype

    Integer, Dimension( : ), Allocatable :: req_list
    
    Integer :: icomm
    Integer :: error

    Call MPI_Type_create_f90_real( Precision( data ), Range( data ), datatype, error )

    ! In the following note the signs on the tags - a send in a psoitive direction
    ! corresponds to a receive from the negative direction
    
    ! Post the recvs
    Do icomm = 1, Size( buffy%recv_buffers )
       Associate( this_recv =>  buffy%recv_buffers( icomm ) )
         Call MPI_Irecv( this_recv%buffer, Size( this_recv%buffer ), datatype, this_recv%remote_rank, &
              dir_to_tag( 3, -this_recv%dir ), buffy%communicator, this_recv%request, error )
       End Associate
    End Do

    ! Do the sends
    Do icomm = 1, Size( buffy%send_buffers )
       Associate( this_send =>  buffy%send_buffers( icomm ) )
         Associate( ilo => this_send%ilo, ihi => this_send%ihi )
           this_send%buffer = data( ilo( 1 ):ihi( 1 ), ilo( 2 ):ihi( 2 ), ilo( 3 ):ihi( 3 ) ) 
         End Associate
         Call MPI_Isend( this_send%buffer, Size( this_send%buffer ), datatype, this_send%remote_rank, &
              dir_to_tag( 3, +this_send%dir ), buffy%communicator, this_send%request, error )
       End Associate
    End Do

    ! Wait for recvs to complete
    req_list = [ buffy%recv_buffers( : )%request ]
    ! Wait for each to complete in turn - you never know we might be able
    ! to overlap the buffer copying with some of the message passing
    Do
       Call MPI_Waitany( Size( req_list ), req_list, icomm, MPI_STATUS_IGNORE, error )
       If( icomm == MPI_UNDEFINED ) Exit
       Associate( this_recv =>  buffy%recv_buffers( icomm ) )
         Associate( ilo => this_recv%ilo, ihi => this_recv%ihi )
           data( ilo( 1 ):ihi( 1 ), ilo( 2 ):ihi( 2 ), ilo( 3 ):ihi( 3 ) ) = this_recv%buffer
         End Associate
       End Associate
    End Do

    ! And finally wait for the sends to complete
    req_list = [ buffy%send_buffers( : )%request ]
    Call MPI_Waitall( Size( req_list ), req_list, MPI_STATUSES_IGNORE, error )

  Contains

    Pure Function dir_to_tag( base, dir ) Result( tag )

      Integer :: tag

      Integer,                   Intent( In ) :: base
      Integer, Dimension( 1:3 ), Intent( In ) :: dir

      tag = Dot_product( dir + 1, [ base * base, base, 1 ] )

      tag = tag + 1
      
    End Function dir_to_tag

  End Subroutine exchange
  
End Module halo_exchange_3d_module
