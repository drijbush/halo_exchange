Program test

  Use numbers_module, Only : wp
  Use mpi           , Only : mpi_init, mpi_comm_rank, mpi_comm_size, &
       mpi_cart_create, mpi_cart_coords, mpi_finalize, MPI_COMM_WORLD, &
       mpi_cart_rank
  Use halo_exchange_3d_module, Only : halo_exchange

  Implicit None

  Integer, Parameter :: w = 3
  Integer, Parameter :: n = 10

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

  Call MPI_Cart_create( MPI_COMM_WORLD, 3, [ 2, 3, 4 ], &
       [ .True., .True., .True. ], .True., new_comm, error )

  Call MPI_Comm_size( new_comm, prcs, error )
  Call MPI_Comm_rank( new_comm, rank, error )
  
  Do k = 0, n - 1
     Do j = 0, n - 1
        Do i = 0, n - 1
           data( i, j, : ) = rank * 1000 + k * 100 + j * 10 + i
        End Do
     End Do
  End Do

  Call mpi_cart_coords( new_comm, rank, 3, my_coords, error )
  Write( *, * ) rank, my_coords

  Do i3 = -1, +1
     Do i2 = -1, +1
        Do i1 = -1, +1
           Call MPI_Cart_rank( new_comm, my_coords + [ i1, i2, i3 ], &
                proc_map( i1, i2, i3 ), error )
           If( rank == 0 ) Then
              Write( *, * ) '!! ', i1, i2, i3, proc_map( i1, i2, i3 )
           End If
        End Do
     End Do
  End Do

  Call buffy%init( [ n, n, n ], w, new_comm, proc_map )
  Call buffy%exchange( [ -w, -w, -w ], data )
  
  Call MPI_Finalize( error )
  
End Program test
