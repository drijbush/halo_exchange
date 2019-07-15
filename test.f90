Program test

  Use numbers_module, Only : wp
  Use mpi           , Only : mpi_init, mpi_comm_rank, mpi_comm_size, &
       mpi_cart_create, mpi_cart_coords, mpi_finalize, MPI_COMM_WORLD, &
       mpi_cart_rank
  Use halo_exchange_3d_module, Only : halo_exchange

  Implicit None

  Integer, Parameter :: w = 2
  Integer, Parameter :: n = 2

  Integer, Parameter :: npx = 2
  Integer, Parameter :: npy = 4
  Integer, Parameter :: npz = 3

  Type( halo_exchange ) :: buffy
  
  Real( wp ), Dimension( -w:n  -1 + w, -w:n  -1 + w, -w:n  -1 + w  ) :: data

  Integer, Dimension( 1:3 ) :: my_coords

  Integer, Dimension( -1:+1, -1:+1, -1:+1 ) :: proc_map

  Integer :: rank, prcs
  Integer :: new_comm
  Integer :: i, j, k
  Integer :: i1, i2, i3
  Integer :: error

  Call MPI_Init( error )
  Call MPI_Comm_size( MPI_COMM_WORLD, prcs, error )
  Call MPI_Comm_rank( MPI_COMM_WORLD, rank, error )

  If( npx * npy * npz == prcs ) Then

     Call MPI_Cart_create( MPI_COMM_WORLD, 3, [ npx, npy, npz ], &
          [ .True., .True., .True. ], .True., new_comm, error )

     Call MPI_Comm_size( new_comm, prcs, error )
     Call MPI_Comm_rank( new_comm, rank, error )
  
     Call mpi_cart_coords( new_comm, rank, 3, my_coords, error )
     Write( *, * ) rank, my_coords

     Do k = 0, n - 1
        Do j = 0, n - 1
           Do i = 0, n - 1
!!$           data( i, j, k ) = rank * 1000 + k * 100 + j * 10 + i
              data( i, j, k ) = ( my_coords( 3 ) * n + k ) * 100 + &
                   ( my_coords( 2 ) * n + j ) * 10 + ( my_coords( 1 ) * n + i )
           End Do
        End Do
     End Do
     
     Do i3 = -1, +1
        Do i2 = -1, +1
           Do i1 = -1, +1
              Call MPI_Cart_rank( new_comm, my_coords + [ i1, i2, i3 ], &
                   proc_map( i1, i2, i3 ), error )
           End Do
           If( rank == 0 ) Then
              Write( *, * ) '!! ', i2, i3, proc_map( :, i2, i3 )
           End If
        End Do
     End Do
  
     Call buffy%init( [ n, n, n ], w, new_comm, proc_map )
     Call buffy%exchange( [ -w, -w, -w ], data )
  
     Do k = -w, n - 1 + w
        If( rank == 0 ) Then
           Write( *, * ) k
        End If
        Do j = -w, n - 1 + w
!!$        Do i = -w, n - 1 + w
           If( rank == 0 ) Then
              Write( *, '( 1( i5, 1x ), 5x, 1000( i5, 1x ) )' ) &
                   j, Int( data( :, j, k ) )
           End If 
!!$        End Do
        End Do
     End Do

  Else

     Write( *, * ) 'Needs to be run on ', npx * npy * npz, ' procs'

  End If

  Call MPI_Finalize( error )
  
End Program test
