## 0.1.12 (2017-12-24)

Features:
---------

  * MPI interface for Nelson
    * MPI\_Initialized : Indicates whether MPI_Init has been called.
    * MPI\_Finalize : Terminates MPI execution environment.
    * MPI\_Init : Initialize the MPI execution environment.
    * MPI\_Get\_processor\_name : Gets the name of the processor.
    * MPI\_Get\_version : Return the version number of MPI.
    * MPI\_Get\_library\_version : Return the version number of MPI library.
    * MPI\_Recv : Blocking receive for a message.
    * MPI\_Send : Performs a blocking send.
    * MPI\_Barrier : Blocks until all processes in the communicator have reached this routine.
    * MPI\_Bcast : Broadcasts a message from the process with rank "root" to all other processes of the communicator.
    * MPI\_Reduce : Reduces values on all processes to a single value.
    * MPI\_Allreduce : Combines values from all processes and distributes the result back to all processes.
    * MPI\_Probe : Blocking test for a message.
    * MPI\_Iprobe : Nonblocking test for a message.
    * MPI\_Comm\_object : Creates MPI_Comm object.
    * MPI\_Comm\_disp : displays MPI_Comm object.
    * MPI\_Comm\_get\_name : Return the print name from the communicator.
    * MPI\_Comm\_delete : delete MPI_Comm object in Nelson environment.
    * MPI\_Comm\_used : get used MPI_Comm objects in Nelson environment.
    * MPI\_Comm\_isvalid : check MPI_Comm handle validity.
    * MPI\_Comm\_split : Creates new communicators based on colors and keys.
    * MPI\_Comm\_rank : Determines the rank of the calling process in the communicator.
    * MPI\_Comm\_size : Determines the size of the group associated with a communicator.
    * mpiexec : Run an MPI script.

Bug Fixes:
---------

  [#85](http://github.com/Nelson-numerical-software/nelson/issues/85): paste in text editor should be limited to the text only.

  [#84](http://github.com/Nelson-numerical-software/nelson/issues/84): CTRL + A action in text editor.

  [#83](http://github.com/Nelson-numerical-software/nelson/issues/83): save action in text editor do a popup to reload file.

  [#82](http://github.com/Nelson-numerical-software/nelson/issues/82): home, end, page down, page up keys shorcut not implement in editor.

Compilation:
---------

* Update VS 2017 solution to VS 2017 15.5.1

## 0.1.11 alpha (2017-11-25)

Features:
---------

  * [#75](http://github.com/Nelson-numerical-software/nelson/issues/75): Intel Math Kernel Library can be used to replace OpenBLAS and FFTW on Windows.

  * [#72](http://github.com/Nelson-numerical-software/nelson/issues/72): Add a script to check missing help files.

  * add an embedded script editor for Nelson:
    * 'edit' builtin added. 
    * syntax colorization.
    * text completion.
    * smart indentation.
    * drag & drop files.
    * files modification notified.
    * files association: .nls .nlf

  * add 'smartindent' builtin to indent a .nls or .nlf files.

  * drag & drop .nls (run), .nlf (open with editor) in gui main window.

Bug Fixes:
---------

  [#80](http://github.com/Nelson-numerical-software/nelson/issues/80):  code editor: keys up, down, left, right behavior

  [#78](http://github.com/Nelson-numerical-software/nelson/issues/78): [p,f,e]=fileparts('c:/') did not return the good result


Compilation:
---------

* BOOST 1.65.1 on Windows


## 0.1.10 alpha (2017-10-29)

Features:
---------

* SLICOT module:
  * SLICOT (Subroutine Library in Systems and Control Theory) builtin generated with NIG.

  * slicot_ab01od : Staircase form for multi-input systems using orthogonal state and input transformations.
  * slicot_ab04md : Discrete-time / continuous-time systems conversion by a bilinear transformation.
  * slicot_ab07nd : Inverse of a given linear system.
  * slicot_ab08nd : Construction of a regular pencil for a given system such that its generalized eigenvalues are invariant zeros of the system.
  * slicot_ag08bd : Zeros and Kronecker structure of a descriptor system pencil.
  * slicot_mb02md : Solution of Total Least-Squares problem using a SVD approach.
  * slicot_mb03od : Matrix rank determination by incremental condition estimation.
  * slicot_mb03pd : Matrix rank determination by incremental condition estimation (row pivoting).
  * slicot_mb03rd : Reduction of a real Schur form matrix to a block-diagonal form.
  * slicot_mb04gd : RQ factorization with row pivoting of a matrix.
  * slicot_mb04md : Balancing a general real matrix.
  * slicot_mb05od : Matrix exponential for a real matrix, with accuracy estimate.
  * slicot_mc01td : Checking stability of a given real polynomial.
  * slicot_sb01bd : Pole assignment for a given matrix pair (A,B).
  * slicot_sb02od : Solution of continuous- or discrete-time algebraic Riccati equations (generalized Schur vectors method).
  * slicot_sb03md : Solution of continuous- or discrete-time Lyapunov equations and separation estimation.
  * slicot_sb03od : Solution of stable continuous- or discrete-time Lyapunov equations (Cholesky factor).
  * slicot_sb04md : Solution of continuous-time Sylvester equations (Hessenberg-Schur method).
  * slicot_sb04qd : Solution of discrete-time Sylvester equations (Hessenberg-Schur method).
  * slicot_sb10jd : Converting a descriptor state-space system into regular state-space form.
  * slicot_sg02ad : Solution of continuous- or discrete-time algebraic Riccati equations for descriptor systems.
  * slicot_tb01id : Balancing a system matrix corresponding to a triplet (A, B, C).
  * slicot_tg01ad : Balancing the matrices of the system pencil corresponding to a descriptor triple (A-lambda E, B, C).

* Nelson Interface Generator (NIG) allows to generate Nelson builtin from C/Fortran code.

* add 'isfield' function to check existence of a fieldname in a struct.

* add 'exist' function to check existence of variable, builtin, function, file or directory.

* add 'isvar' function to check existence of variable.

* add 'ismacro' function to check existence of macro.

* add 'isbuiltin' function to check existence of builtin.

* add 'f2c' function to convert fortran code to C from Nelson

* optimize 'vertcat' and 'horzcat' builtin (remove duplicated code).

* history file saved on your cloud drive if 'OneDrive' environment variable defined.

Bug Fixes:
---------

  [#74](http://github.com/Nelson-numerical-software/nelson/issues/74): A=[]; A(end + 1) = 8 failed.

  [#71](http://github.com/Nelson-numerical-software/nelson/issues/71): unix & dos did not have help files.

  [#68](http://github.com/Nelson-numerical-software/nelson/issues/68): display of nd array of integer were not defined.

  [#14](http://github.com/Nelson-numerical-software/nelson/issues/14): error(error_struct) throws an error using the fields of error_struct.


Compilation:
---------

* SLICOT used for control functions.

* add Fortran 2 C converter library (based on libf2c forked).


## 0.1.9 alpha (2017-09-02)


Features:
---------

* add 'fftshift' and 'ifftshift' functions to shift the zero-frequency component to the center of the spectrum.

* add 'circshift' function.

* colon as string managed for deletion, extraction and insertion.

* add 'rem' builtin. Remainder after division.

* add 'abs' builtin. Absolute value.

* add 'repmat' builtin. Replicates and tiles an array.

* add 'mod' builtin. Modulus after division.

* add 'maxNumCompThreads' builtin. Set/Get maximum number of computional threads.


Compilation:
---------

* Nelson Visual Studio solution updated to use 2017 version.

* On Windows, all dependencies updated to be compatible with VS 2017 runtime.

* more 50 warnings fixed (Thanks to PVS-Studio analyzer and also Cppcheck).


Bug Fixes:
---------

  [#60](http://github.com/Nelson-numerical-software/nelson/issues/60): Y = complex(rand(500), rand(500)) crashed Nelson.

  [#58](http://github.com/Nelson-numerical-software/nelson/issues/58): Manages ICU4C 59.1 on macos X.


## 0.1.8 alpha (2017-08-15)


Features:
---------

* FFTW module:
  * fft, ifft, fftn, ifftn functions based on FFTW library.
  * fftw function to manage FFTW wisdom data.

* prod : product of array elements builtin added.

* conj : complex conjugate builtin added.

* transpose and complex conjugate transpose manage all types available in Nelson.

* APPVEYOR updated to build windows 32 & 64 bits versions.


## 0.1.7 alpha (2017-07-16)

Features:
---------

* On Windows, Nelson can read/write all excel 97-2016 file formats (Excel required) based on COM: 
  * COM_xlsread : read a xls/xlsx file.
  * COM_xlswrite : write a xls/xlsx file.
  * COM_xlsfinfo : get informations about xls/xlsx file.

* Component Object Model (COM) client interface: binary-interface standard for software components on Windows. 
  * actxcontrollist : get list all available control services installed on current Windows. 
  * actxserverlist : get list all available active X services installed on current Windows. 
  * actxGetRunningServer : get COM handle of an existing COM server.
  * actxserver : creates a COM server.
  * COM_used : get list of COM handle currently used in current Nelson's session.
  * iscom : determines if it is a COM handle. 
  * overload on : class, delete, disp, fieldnames, get, set, invoke, ismethod, isprop,  isvalid, methods. 
  * add COM handle. 
  * see examples in [modulepath('com_engine'), '/examples/'] directory.

* Better management of varargout without output. 
* Add "<--EXCEL REQUIRED-->" tag managed by test engine.

Bug Fixes:
---------

  [#53](http://github.com/Nelson-numerical-software/nelson/issues/53): COM_xlsread did not support path with dot '.'

  [#50](http://github.com/Nelson-numerical-software/nelson/issues/50): qml demos did not start on adv-cli mode.


## 0.1.6 alpha (2017-06-19)

Features:
---------

* The QML engine enables nelson programs to display and manipulate graphical content using Qt's QML framework.

  ```
  qml_demos // for demonstrations
  ```
* Qt 5.9 used on Windows binaries
* add CONTRIBUTING.md , CODE\_OF\_CONDUCT.md and ROADMAP.md files
* add 'heldlg', 'msgbox', 'errordlg', 'warndlg' functions based on QML.
* add 'questdlg' function (Creates a question dialog box) based on QML.
* add '==', '~=', 'isequal', 'isequaln' overload for handle and QObject.
* add ishandle, isprop, ismethod, method, properties builtin.
* add set, get, invoke, isvalid builtin used by handle objects.
* extends clear function to call delete if 'TYPEHANDLE\_clear' function is defined.
* add handle object type.
* gui module also loaded in advanced cli mode.
* add 'sind', 'cosd', 'tand' functions.
* add more output information in the result file of test_run function.

Bug Fixes:
---------

  [#47](http://github.com/Nelson-numerical-software/nelson/issues/47): add isfinite builtin.

  [#43](http://github.com/Nelson-numerical-software/nelson/issues/43): rename getContentsAsWideString to getContentAsWideString.

  [#27](http://github.com/Nelson-numerical-software/nelson/issues/27): >= operator was not implemented.


## 0.1.5 alpha (2017-04-17)

Features:
---------

* Optimize constructors allocation for eye, ones, Inf, NaN, rand, randn functions.
* Add 'issymmetric' function.
* Add 'svd' function.
* Add 'rcond' function.

Bug Fixes:
---------

  [#41](http://github.com/Nelson-numerical-software/nelson/issues/41): test_makeref starts a new clear session to create a ref file.

  [#39](http://github.com/Nelson-numerical-software/nelson/issues/39): inv([0 0;i() 3]) did not return [Inf, Inf; Inf, Inf] on ARM platform.

  [#25](http://github.com/Nelson-numerical-software/nelson/issues/25): ndims added.

  [#24](http://github.com/Nelson-numerical-software/nelson/issues/24): isnan & isinf added.


## 0.1.4 alpha (2017-03-19)

Compilation:
---------

  LAPACKE used for linear algebra functions. On Windows, OpenBLAS used. On others platforms reference library LAPACKE used.

Features:
---------

* Add 'isnan' function.
* Rework double and single matrix 2D display.
* Add 'trace' function.
* Add 'schur' function.
* Add 'inv' function.
* Add 'expm' function (single, double, sparse double managed).
* Rename 'string' type to 'char' type, 'string' type will be added later.
* Add 'dlmwrite' function.
* Add 'mat2str' function (sparse managed).
* Add 'str2double' function.
* Add 'cosm' function.
* which(function_name, '-all') without output argument display paths.

Bug Fixes:
---------

  [#34](http://github.com/Nelson-numerical-software/nelson/issues/34): Windows installer did not copy module_skeleton help files at the good place.

  [#31](http://github.com/Nelson-numerical-software/nelson/issues/31): doc function was too slow (indexing at each restart).

  [#30](http://github.com/Nelson-numerical-software/nelson/issues/30): Search path order for functions was wrong.


## 0.1.3 alpha (2017-02-11)

Features:
---------

* datevec updated to manage vectors, matrix.
* Add 'nnz', 'nzmax', 'numel' functions.
* Rework 'searchenv' to return a cell of strings.
* Update persistent variables behavior (clear functions).
* Add 'ceil', 'floor', 'round', 'fix' functions.
* Add sparse support for acos, asin, atan, cos, cosh, sin, sinh, tan, tanh.
* Add website and bug reports link in help menu.

Bug Fixes:
---------

  [#28](http://github.com/Nelson-numerical-software/nelson/issues/28): Each public builtin have a help file.
  
  [#26](http://github.com/Nelson-numerical-software/nelson/issues/26): 7.853981633974482790D-01 was not correctly parsed.

  [#22](http://github.com/Nelson-numerical-software/nelson/issues/22): last output argument of 'IJV' did not return nzmax.

  [#19](http://github.com/Nelson-numerical-software/nelson/issues/19): rand & randn did not use Column-major order.

  [#17](http://github.com/Nelson-numerical-software/nelson/issues/17): 'locales' directory renamed as 'locale' (more standard).

  [#13](http://github.com/Nelson-numerical-software/nelson/issues/13): mldivide and mrdivide builtin were missing.


## 0.1.2 alpha (2017-01-01)

Bug Fixes:
---------

  [#11](http://github.com/Nelson-numerical-software/nelson/issues/11): colon constructor failed with some special values.
  
  [#10](http://github.com/Nelson-numerical-software/nelson/issues/10): module_skeleton did not build/load.
  
  [#8](http://github.com/Nelson-numerical-software/nelson/issues/8): test_nargin & test_nargout failed in Windows binary version.
  

## 0.1.1 alpha (2016-12-30)

Bug Fixes:
---------
  [#5](http://github.com/Nelson-numerical-software/nelson/issues/5): help browser did not work on some Windows

## 0.1.0 alpha (2016-12-28)
* Initial import in github of Nelson
