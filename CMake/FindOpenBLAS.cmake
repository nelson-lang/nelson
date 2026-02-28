if(WIN32)
	# Which compilers to use for C and C++, and location of target
	# environment.
	execute_process(COMMAND uname -m OUTPUT_VARIABLE ARCH)
	message("ARCH IS ${ARCH} and PROCESSOR ${CMAKE_SYSTEM_PROCESSOR}")
	if(${ARCH} EQUAL "x86_64")
		# First look in standard location as used by Debian/Ubuntu/etc.
		set(MINGW_ROOT /mingw64)
		message("64 bit!")
	else()
		# First look in standard location as used by Debian/Ubuntu/etc.
		set(MINGW_ROOT /mingw32)
		message("32 bit")

	endif()

	set(Open_BLAS_INCLUDE_SEARCH_PATHS
		${MINGW_ROOT}/include
	)

	set(Open_BLAS_LIB_SEARCH_PATHS
		${MINGW_ROOT}/lib/
	)
	message("CMAKE MINGW ROOT ${MINGW_ROOT}")

else()
	set(Open_BLAS_INCLUDE_SEARCH_PATHS
		/usr/include
		/usr/include/openblas
		/usr/include/openblas-base
		/usr/local/include
		/usr/local/include/openblas
		/usr/local/include/openblas-base
		/usr/local/opt/openblas/include
		/opt/OpenBLAS/include
	)

	if(NOT
		DEFINED
		ENV{IN_NIX_SHELL}
		AND
		NOT
		DEFINED
		ENV{CONDA_PREFIX}
		AND
		DEFINED
		ENV{HOMEBREW_PREFIX}
	)
		list(APPEND Open_BLAS_INCLUDE_SEARCH_PATHS
			"$ENV{HOMEBREW_PREFIX}/opt/openblas/include"
		)
	endif()

	set(Open_BLAS_LIB_SEARCH_PATHS
		/lib/
		/lib/openblas-base
		/lib64/
		/usr/lib
		/usr/lib/openblas-base
		/usr/lib64
		/usr/local/lib
		/usr/local/lib64
		/usr/local/opt/openblas/lib
		/opt/OpenBLAS/lib
	)
	if(NOT
		DEFINED
		ENV{IN_NIX_SHELL}
		AND
		NOT
		DEFINED
		ENV{CONDA_PREFIX}
		AND
		DEFINED
		ENV{HOMEBREW_PREFIX}
	)
		list(APPEND Open_BLAS_LIB_SEARCH_PATHS
			"$ENV{HOMEBREW_PREFIX}/opt/openblas/lib"
		)
	endif()
endif()

find_path(OpenBLAS_INCLUDE_DIR
	NAMES
	cblas.h
	PATHS
	${Open_BLAS_INCLUDE_SEARCH_PATHS}
)
find_library(OpenBLAS_LIB NAMES openblas PATHS ${Open_BLAS_LIB_SEARCH_PATHS})

set(OpenBLAS_FOUND ON)

# Check include files
if(NOT OpenBLAS_INCLUDE_DIR)
	set(OpenBLAS_FOUND OFF)
	message(STATUS "Could not find OpenBLAS include. Turning OpenBLAS_FOUND off")
endif()

# Check libraries
if(NOT OpenBLAS_LIB)
	set(OpenBLAS_FOUND OFF)
	message(STATUS "Could not find OpenBLAS lib. Turning OpenBLAS_FOUND off")
endif()

if(OpenBLAS_FOUND)
	if(NOT OpenBLAS_FIND_QUIETLY)
		message(STATUS "Found OpenBLAS libraries: ${OpenBLAS_LIB}")
		message(STATUS "Found OpenBLAS include: ${OpenBLAS_INCLUDE_DIR}")
	endif()
else()
	if(OpenBLAS_FIND_REQUIRED)
		message(FATAL_ERROR "Could not find OpenBLAS")
	endif()
endif()

mark_as_advanced(OpenBLAS_INCLUDE_DIR
	OpenBLAS_LIB
	OpenBLAS
)
