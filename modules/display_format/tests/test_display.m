%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
format('short')
format('loose')
%=============================================================================
assert_isequal(nargin('display'), 2)
assert_isequal(nargout('display'), 0)
%=============================================================================
X = 'Alice will be 12 this year.';
R = evalc('display(X, ''YYY'')');
REF = '
YYY =

    ''Alice will be 12 this year.''

';
assert_isequal(R, REF);
%=============================================================================
A = 'hello';
R = evalc('display(A)');
REF = '
A =

    ''hello''

';
assert_isequal(R, REF);
%=============================================================================
A = 'hello';
R = evalc('disp(A)');
REF = 'hello
';
assert_isequal(R, REF);
%=============================================================================
A = ['fa'; 'af'];
R = evalc('display(A)');
REF = '
A =

  2×2 char array

    ''fa''
    ''af''

';
assert_isequal(R, REF);
%=============================================================================
A = ['fa'; 'af'];
R = evalc('disp(A)');
REF ='fa
af
';
assert_isequal(R, REF);
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
assert_isequal(R, REF);
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
assert_isequal(R, REF);
%=============================================================================
A = char(ones(3, 0, 1));
R = evalc('display(A)');
REF = '
A =

  3×0 empty char array

';
assert_isequal(R, REF);
%=============================================================================
A = char(ones(3, 0, 1));
R = evalc('disp(A)');
REF = char(ones(0, 0));
assert_isequal(R, REF);
%=============================================================================
A = {};
R = evalc('display(A)');
REF = '
A =

  0×0 empty cell array

';
assert_isequal(R, REF);
%=============================================================================
A = {};
R = evalc('disp(A)');
REF = char(ones(0, 0));
assert_isequal(R, REF);
%=============================================================================
A = logical([]);
R = evalc('display(A)');
REF = '
A =

  0×0 empty logical array

';
assert_isequal(R, REF);
%=============================================================================
A = logical([]);
R = evalc('disp(A)');
REF = char(ones(0, 0));
assert_isequal(R, REF);
%=============================================================================
A = double([]);
R = evalc('display(A)');
REF = '
A =

     []

';
assert_isequal(R, REF);
%=============================================================================
A = double([]);
R = evalc('disp(A)');
REF = char(ones(0, 0));
assert_isequal(R, REF);
%=============================================================================
A = ones(3, 0, 2);
R = evalc('display(A)');
REF = '
A =

  3×0×2 empty double array

';
assert_isequal(R, REF);
%=============================================================================
A = ones(3,0, 2);
R = evalc('disp(A)');
REF = char(ones(0, 0));
assert_isequal(R, REF);
%=============================================================================
A = eye(2, 2);
R = evalc('display(A)');
REF = '
A =

     1     0
     0     1

';
assert_isequal(R, REF);
%=============================================================================
A = eye(2, 2);
R = evalc('disp(A)');
REF = '     1     0
     0     1

';
assert_isequal(R, REF);
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
assert_isequal(R, REF);
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
assert_isequal(R, REF);
%=============================================================================
A = struct([]);
R = evalc('display(A)');
REF =  '
A =

  0×0 empty struct array with no fields.

';
assert_isequal(R, REF);
%=============================================================================
A = struct([]);
R = evalc('disp(A)');
REF = char(ones(0, 0));
assert_isequal(R, REF);
%=============================================================================
A = struct(ones(3,0,2));
R = evalc('display(A)');
REF = '
A =

  3×0×2 empty struct array with no fields.

';
assert_isequal(R, REF);
%=============================================================================
A = struct(ones(3,0,2));
R = evalc('disp(A)');
REF = char(ones(0, 0));
assert_isequal(R, REF);
%=============================================================================
A = []; A.a=1; A.b='f'; A.c="d"; A.d = {'f', 'f'};
R = evalc('display(A)');
REF = '
A =

  struct with fields:

    a: 1
    b: ''f''
    c: "d"
    d: {''f''  ''f''}

';
assert_isequal(R, REF);
%=============================================================================
A = []; A.a=1; A.b='f'; A.c="d"; A.d = {'f', 'f'};
R = evalc('disp(A)');
REF = '    a: 1
    b: ''f''
    c: "d"
    d: {''f''  ''f''}

';
assert_isequal(R, REF);
%=============================================================================
A = [];
A.a=1;
A.b='f';
A.c="d";
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
    f: {[1  2]  3  []}

';
assert_isequal(R, REF);
%=============================================================================
A = int64([]);
R = evalc('display(A)');
REF = '
A =

  0×0 empty int64 matrix

';
assert_isequal(R, REF);
%=============================================================================
A = int64([]);
R = evalc('disp(A)');
REF = char(ones(0, 0));
assert_isequal(R, REF);
%=============================================================================
A = int64(ones(3,0,2));
R = evalc('display(A)');
REF =  '
A =

  3×0×2 empty int64 array

';
assert_isequal(R, REF);
%=============================================================================
A = int64(ones(3,0,2));
R = evalc('disp(A)');
REF = char(ones(0, 0));
assert_isequal(R, REF);
%=============================================================================
A = '';
R = evalc('display(A)');
REF = '
A =

  0×0 empty char array

';
assert_isequal(R, REF);
%=============================================================================
A = '';
R = evalc('disp(A)');
REF = char(ones(0, 0));
assert_isequal(R, REF);
%=============================================================================
