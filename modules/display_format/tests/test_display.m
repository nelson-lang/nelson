%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
format('short')
format('loose')
%=============================================================================
asserts.isequal(nargin('display'), 2)
asserts.isequal(nargout('display'), 0)
%=============================================================================
X = 'Alice will be 12 this year.';
R = evalc('display(X, ''YYY'')');
REF = '
YYY =

    ''Alice will be 12 this year.''

';
asserts.isequal(R, REF);
%=============================================================================
A = 'hello';
R = evalc('display(A)');
REF = '
A =

    ''hello''

';
asserts.isequal(R, REF);
%=============================================================================
A = 'hello';
R = evalc('disp(A)');
REF = 'hello
';
asserts.isequal(R, REF);
%=============================================================================
A = ['fa'; 'af'];
R = evalc('display(A)');
REF = '
A =

  2×2 char array

    ''fa''
    ''af''

';
asserts.isequal(R, REF);
%=============================================================================
A = ['fa'; 'af'];
R = evalc('disp(A)');
REF ='fa
af
';
asserts.isequal(R, REF);
%=============================================================================
A = char(ones(3, 2, 2) * 101);
R = evalc('display(A)');
REF = '
  3×2×2 char array

A(:,:,1) =

    ''ee''
    ''ee''
    ''ee''

A(:,:,2) =

    ''ee''
    ''ee''
    ''ee''

';
asserts.isequal(R, REF);
%=============================================================================
A = char(ones(3, 2, 2) * 101);
R = evalc('disp(A)');
REF = '
(:,:,1) =

ee
ee
ee

(:,:,2) =

ee
ee
ee
';
asserts.isequal(R, REF);
%=============================================================================
A = char(ones(3, 0, 1));
R = evalc('display(A)');
REF = '
A =

  3×0 empty char array

';
asserts.isequal(R, REF);
%=============================================================================
A = char(ones(3, 0, 1));
R = evalc('disp(A)');
REF = char(ones(0, 0));
asserts.isequal(R, REF);
%=============================================================================
A = {};
R = evalc('display(A)');
REF = '
A =

  0×0 empty cell array

';
asserts.isequal(R, REF);
%=============================================================================
A = {};
R = evalc('disp(A)');
REF = char(ones(0, 0));
asserts.isequal(R, REF);
%=============================================================================
A = logical([]);
R = evalc('display(A)');
REF = '
A =

  0×0 empty logical array

';
asserts.isequal(R, REF);
%=============================================================================
A = logical([]);
R = evalc('disp(A)');
REF = char(ones(0, 0));
asserts.isequal(R, REF);
%=============================================================================
A = double([]);
R = evalc('display(A)');
REF = '
A =

     []

';
asserts.isequal(R, REF);
%=============================================================================
A = double([]);
R = evalc('disp(A)');
REF = char(ones(0, 0));
asserts.isequal(R, REF);
%=============================================================================
A = ones(3, 0, 2);
R = evalc('display(A)');
REF = '
A =

  3×0×2 empty double array

';
asserts.isequal(R, REF);
%=============================================================================
A = ones(3, 0, 2);
R = evalc('disp(A)');
REF = char(ones(0, 0));
asserts.isequal(R, REF);
%=============================================================================
A = eye(2, 2);
R = evalc('display(A)');
REF = '
A =

     1     0
     0     1

';
asserts.isequal(R, REF);
%=============================================================================
A = eye(2, 2);
R = evalc('disp(A)');
REF = '     1     0
     0     1

';
asserts.isequal(R, REF);
%=============================================================================
A = ones(2, 1, 2);
R = evalc('display(A)');
REF = '
A(:,:,1) =

     1
     1


A(:,:,2) =

     1
     1

';
asserts.isequal(R, REF);
%=============================================================================
A = ones(2, 1, 2);
R = evalc('disp(A)');
REF = '
(:,:,1) =

     1
     1


(:,:,2) =

     1
     1

';
asserts.isequal(R, REF);
%=============================================================================
A = struct([]);
R = evalc('display(A)');
REF =  '
A = 

  0×0 empty struct array with no fields.

';
asserts.isequal(R, REF);
%=============================================================================
A = struct([]);
R = evalc('disp(A)');
REF = '';
asserts.isequal(R, REF);
%=============================================================================
A = struct(ones(3, 0, 2));
R = evalc('display(A)');
REF = '
A = 

  3×0×2 empty struct array with no fields.

';
asserts.isequal(R, REF);
%=============================================================================
A = struct(ones(3, 0, 2));
R = evalc('disp(A)');
REF = '';
asserts.isequal(R, REF);
%=============================================================================
A = []; A.a = 1; A.b = 'f'; A.c = "d"; A.d = {'f', 'f'};
R = evalc('display(A)');
REF = '
A = 

  struct with fields:

    a: 1
    b: ''f''
    c: "d"
    d: {''f''  ''f''}

';
asserts.isequal(R, REF);
%=============================================================================
A = []; A.a = 1; A.b = 'f'; A.c = "d"; A.d = {'f', 'f'};
R = evalc('disp(A)');
REF = '    a: 1
    b: ''f''
    c: "d"
    d: {''f''  ''f''}

';
asserts.isequal(R, REF);
%=============================================================================
A = [];
A.a = 1;
A.b = 'f';
A.c = "d";
A.d = {'fdfddddd', 'fddddddddddddddddd', {'mmmmm'}};
A.e = ["a", "c"];
A.f = {[1, 2], 3, []};
R = evalc('display(A)');
REF = '
A = 

  struct with fields:

    a: 1
    b: ''f''
    c: "d"
    d: {''fdfddddd''  ''fddddddddddddddddd''  {1×1 cell}}
    e: ["a"    "c"]
    f: {[1 2]  [3]  []}

';
asserts.isequal(R, REF);
%=============================================================================
A = int64([]);
R = evalc('display(A)');
REF = '
A =

  0×0 empty int64 matrix

';
asserts.isequal(R, REF);
%=============================================================================
A = int64([]);
R = evalc('disp(A)');
REF = char(ones(0, 0));
asserts.isequal(R, REF);
%=============================================================================
A = int64(ones(3, 0, 2));
R = evalc('display(A)');
REF =  '
A =

  3×0×2 empty int64 array

';
asserts.isequal(R, REF);
%=============================================================================
A = int64(ones(3, 0, 2));
R = evalc('disp(A)');
REF = char(ones(0, 0));
asserts.isequal(R, REF);
%=============================================================================
A = '';
R = evalc('display(A)');
REF = '
A =

  0×0 empty char array

';
asserts.isequal(R, REF);
%=============================================================================
A = '';
R = evalc('disp(A)');
REF = char(ones(0, 0));
asserts.isequal(R, REF);
%=============================================================================
R = evalc('disp(true),disp(false),disp(true)');
REF = '     true

    false

     true

';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('disp(''-''),disp(''-''),disp(''-'')');
REF = '-
-
-
';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('display(logical([]),'''');');
REF = '  0×0 empty logical array

';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('display([],'''');');
REF = '     []

';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('display(complex([]),'''');');
REF = '  0×0 empty complex double matrix

';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('display(single([]),'''');');
REF = '  0×0 empty single matrix

';
asserts.isequal(R, REF);
%=============================================================================
A = {sparse(logical(ones(2, 2)))};
R = evalc('A');
REF = '
A =

  1×1 cell array

    {2×2 logical}

';
asserts.isequal(R, REF);
%=============================================================================
A = {sparse(complex([]))};
R = evalc('A');
REF = '
A =

  1×1 cell array

    {0×0 double}

';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('disp(int8(3)), disp(int32(4)), disp(int64(5))');
REF =     '   3

   4

   5

';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('disp(single(3i)), disp(4i), disp(5i)');
REF = '   0.0000 + 3.0000i

   0.0000 + 4.0000i

   0.0000 + 5.0000i

';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('disp(sparse(complex([])));disp(sparse(complex([])))');
REF = '';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('disp(sparse(complex(1)));disp(sparse(complex(1)))');
REF = '   (1,1)      1.0000 + 0.0000i

   (1,1)      1.0000 + 0.0000i

';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('disp(sparse(1));disp(sparse(1))');
REF = '   (1,1)        1

   (1,1)        1

';
asserts.isequal(R, REF);
%=============================================================================
st = []; st.a = 3;
R = evalc('disp(st);disp(st)');
REF = '    a: 3

    a: 3

';
asserts.isequal(R, REF);
%=============================================================================
