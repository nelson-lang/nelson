%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('str2double'), 1);
assert_isequal(nargout('str2double'), 1);
%=============================================================================
assert_isequal(str2double(''), NaN);
assert_isequal(str2double('NaN'), NaN);
assert_isequal(str2double('NAN'), NaN);
assert_isequal(str2double('Nan'), NaN);
assert_isequal(str2double('-NaN'), NaN);
assert_isequal(str2double('-NAN'), NaN);
assert_isequal(str2double('-Nan'), NaN);
assert_isequal(str2double('+NaN'), NaN);
assert_isequal(str2double('+NAN'), NaN);
assert_isequal(str2double('+Nan'), NaN);
assert_isequal(str2double('Inf'), Inf);
assert_isequal(str2double('INf'), Inf);
assert_isequal(str2double('+Inf'), Inf);
assert_isequal(str2double('+INf'), Inf);
assert_isequal(str2double('-Inf'), -Inf);
assert_isequal(str2double('-INf'), -Inf);
%=============================================================================
assert_isequal(str2double('10') , 10);
assert_isequal(str2double('+10') , 10);
assert_isequal(str2double('-10') , -10);
assert_isequal(str2double('10.') , 10);
assert_isequal(str2double('+10.') , 10);
assert_isequal(str2double('-10.') , -10);
assert_isequal(str2double('.4') , .4);
assert_isequal(str2double('+.4') , .4);
assert_isequal(str2double('-.44') , -.44);
assert_isequal(str2double('10.4') , 10.4);
assert_isequal(str2double('+10.44') , 10.44);
assert_isequal(str2double('-10.44') , -10.44);
%=============================================================================
assert_isequal(str2double('123.45e7'), 123.45e7);
assert_isequal(str2double('3.14159'), 3.14159);
assert_isequal(str2double('1,200.34'), 1200.34);
%=============================================================================
assert_isequal(str2double('4e2'), 400);
assert_isequal(str2double('-4e2'), -400);
assert_isequal(str2double('+4e2'), 400);
assert_isequal(str2double('0.4e2'), 40);
%=============================================================================
assert_isequal(str2double('3d2') , 300);
assert_isequal(str2double('3D2') , 300);
assert_isequal(str2double('3e2') , 300);
assert_isequal(str2double('3E2') , 300);
%=============================================================================
assert_isequal(str2double('3 2') , NaN);
assert_isequal(str2double('  2') , 2);
assert_isequal(str2double('2  ') , 2);
assert_isequal(str2double('  2  ') , 2);
%=============================================================================
assert_isequal(str2double({'3', 3; 5, '5'}), [3, NaN; NaN, 5]);
%=============================================================================
assert_isequal(str2double('+i'), i);
assert_isequal(str2double('-i'), -i);
assert_isequal(str2double('+j'), i);
assert_isequal(str2double('-j'), -i);
assert_isequal(str2double('i'), i);
assert_isequal(str2double('j'), i);
%=============================================================================
assert_isequal(str2double('I'), NaN);
assert_isequal(str2double('J'), NaN);
%=============================================================================
assert_isequal(str2double(' 3 + 2i'), complex(3, 2));
assert_isequal(str2double(' 3 - 2i'), complex(3, -2));
assert_isequal(str2double(' -3 + 2i'), complex(-3, 2));
assert_isequal(str2double(' 3 - 2i'), complex(3, -2));
assert_isequal(str2double('- 3 - 2i'), complex(-3, -2));
assert_isequal(str2double('+3 - 2i'), complex(+3, -2));
assert_isequal(str2double('+3 + 2i'), complex(3, 2));
assert_isequal(str2double('+3 - 2i'), complex(3, -2));
%=============================================================================
assert_isequal(str2double('4+i'), complex(4, 1));
assert_isequal(str2double('4-i'), complex(4, -1));
assert_isequal(str2double('-4+i'), complex(-4, 1));
assert_isequal(str2double('-4-i'), complex(-4, -1));
assert_isequal(str2double('+4+i'), complex(4, 1));
assert_isequal(str2double('+4-i'), complex(4, -1));
%=============================================================================
assert_isequal(str2double('10+10j'), complex(10, 10));
assert_isequal(str2double('+10+10j'), complex(10, 10));
assert_isequal(str2double('-10+10j'), complex(-10, 10));
assert_isequal(str2double('10.+10j'), complex(10, 10));
assert_isequal(str2double('+10.+10j'), complex(10, 10));
assert_isequal(str2double('-10.+10j'), complex(-10, 10));
assert_isequal(str2double('10.4+10j'), complex(10.4, 10));
assert_isequal(str2double('+10.4+10j'), complex(10.4, 10));
assert_isequal(str2double('-10.4+10j'), complex(-10.4, 10));
assert_isequal(str2double('.4+10j'), complex(.4, 10));
assert_isequal(str2double('+.4+10j'), complex(.4, 10));
assert_isequal(str2double('-.4+10j'), complex(-.4, 10));
assert_isequal(str2double('10+10.4j'), complex(10, 10.4));
assert_isequal(str2double('10.4+10.4j'), complex(10.4, 10.4));
assert_isequal(str2double('+10.4+10.4j'), complex(10.4, 10.4));
assert_isequal(str2double('+10.4-10.4j'), complex(10.4, -10.4));
%=============================================================================
assert_isequal(str2double('1i'), i);
assert_isequal(str2double('1j'), i);
assert_isequal(str2double('+1j'), i);
assert_isequal(str2double('+1i'), i);
assert_isequal(str2double('-1j'), complex(0,-1));
assert_isequal(str2double('-1i'), complex(0,-1));
assert_isequal(str2double('10j'), complex(0,10));
assert_isequal(str2double('+10j'), complex(0,10));
assert_isequal(str2double('-10j'), complex(0, -10));
assert_isequal(str2double('10.j'), complex(0,10));
assert_isequal(str2double('+10.j'), complex(0,10));
assert_isequal(str2double('-10.j'), complex(0,-10));
assert_isequal(str2double('.4j'), complex(0,.4));
assert_isequal(str2double('+.4j'), complex(0,.4));
assert_isequal(str2double('-.4j'), complex(0,-.4));
assert_isequal(str2double('10.4j'), complex(0,10.4));
assert_isequal(str2double('+10.4j'), complex(0,10.4));
assert_isequal(str2double('-10.4j') , complex(0,-10.4));
%=============================================================================
assert_isequal(str2double('3e2 + 2e3i'), complex(300, 2000));
assert_isequal(str2double('3e2 - 2e3i'), complex(300, -2000));
assert_isequal(str2double('3e2 - 2d3i'), complex(300, -2000));
assert_isequal(str2double('3e2 + 2d3i'), complex(300, 2000));
assert_isequal(str2double('3d2 + 2e3i'), complex(300, 2000));
assert_isequal(str2double('3d2 - 2e3i'), complex(300, -2000));
assert_isequal(str2double('3d2 + 2d3i'), complex(300, 2000));
assert_isequal(str2double('3d2 - 2d3i'), complex(300, -2000));
%=============================================================================
assert_isequal(str2double('1,2 + 3,4i'), complex(12, 34));
%=============================================================================
ce = {'2.718','3.1416'; '137','0.015'};
X = str2double(ce);
REF =  [ 2.7180    3.1416; 137.0000    0.0150];
assert_isapprox(X, REF, 1e-4);
%=============================================================================
X = str2double('1,,2');
REF = 12;
assert_isequal(X, REF);
%=============================================================================
X = str2double('NaN*i');
REF = complex(NaN, NaN);
assert_isequal(X, REF);
%=============================================================================
X = str2double('Nan*i');
REF = complex(NaN, NaN);
assert_isequal(X, REF);
%=============================================================================
X = str2double('Inf*i');
REF = complex(0, Inf);
assert_isequal(X, REF);
%=============================================================================
X = str2double('Infi');
REF = complex(0, Inf);
assert_isequal(X, REF);
%=============================================================================
X = str2double('INFi');
REF = complex(0, Inf);
assert_isequal(X, REF);
%=============================================================================
X = str2double('-NaN*i');
REF = complex(NaN, NaN);
assert_isequal(X, REF);
%=============================================================================
X = str2double('-Nan*i');
REF = complex(NaN, NaN);
assert_isequal(X, REF);
%=============================================================================
X = str2double('-Inf*i');
REF = complex(0, -Inf);
assert_isequal(X, REF);
%=============================================================================
X = str2double('-Infi');
REF = complex(0, -Inf);
assert_isequal(X, REF);
%=============================================================================
X = str2double('-INFi');
REF = complex(0, -Inf);
assert_isequal(X, REF);
%=============================================================================
X = str2double('+NaN*i');
REF = complex(NaN, NaN);
assert_isequal(X, REF);
%=============================================================================
X = str2double('+Nan*i');
REF = complex(NaN, NaN);
assert_isequal(X, REF);
%=============================================================================
X = str2double('+Inf*i');
REF = complex(0, Inf);
assert_isequal(X, REF);
%=============================================================================
X = str2double('+Infi');
REF = complex(0, Inf);
assert_isequal(X, REF);
%=============================================================================
X = str2double('+INFi');
REF = complex(0, Inf);
assert_isequal(X, REF);
%=============================================================================
X = str2double('InfI');
assert_isequal(X, NaN);
%=============================================================================
ce = ["2.718", "3.1416"; "137", "0.015"];
X = str2double(ce);
REF =  [ 2.7180    3.1416; 137.0000    0.0150];
assert_isapprox(X, REF, 1e-4);
%=============================================================================
