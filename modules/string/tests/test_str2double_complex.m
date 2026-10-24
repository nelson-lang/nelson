%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
asserts.isequal(str2double('+i'), i);
asserts.isequal(str2double('-i'), -i);
asserts.isequal(str2double('+j'), i);
asserts.isequal(str2double('-j'), -i);
asserts.isequal(str2double('i'), i);
asserts.isequal(str2double('j'), i);
%=============================================================================
asserts.isequal(str2double('I'), NaN);
asserts.isequal(str2double('J'), NaN);
%=============================================================================
asserts.isequal(str2double(' 3 + 2i'), complex(3, 2));
asserts.isequal(str2double(' 3 - 2i'), complex(3, -2));
asserts.isequal(str2double(' -3 + 2i'), complex(-3, 2));
asserts.isequal(str2double(' 3 - 2i'), complex(3, -2));
asserts.isequal(str2double('- 3 - 2i'), NaN);
asserts.isequal(str2double('+3 - 2i'), complex(+3, -2));
asserts.isequal(str2double('+3 + 2i'), complex(3, 2));
asserts.isequal(str2double('+3 - 2i'), complex(3, -2));
%=============================================================================
asserts.isequal(str2double('4+i'), complex(4, 1));
asserts.isequal(str2double('4-i'), complex(4, -1));
asserts.isequal(str2double('-4+i'), complex(-4, 1));
asserts.isequal(str2double('-4-i'), complex(-4, -1));
asserts.isequal(str2double('+4+i'), complex(4, 1));
asserts.isequal(str2double('+4-i'), complex(4, -1));
%=============================================================================
asserts.isequal(str2double('10+10j'), complex(10, 10));
asserts.isequal(str2double('+10+10j'), complex(10, 10));
asserts.isequal(str2double('-10+10j'), complex(-10, 10));
asserts.isequal(str2double('10.+10j'), complex(10, 10));
asserts.isequal(str2double('+10.+10j'), complex(10, 10));
asserts.isequal(str2double('-10.+10j'), complex(-10, 10));
asserts.isequal(str2double('10.4+10j'), complex(10.4, 10));
asserts.isequal(str2double('+10.4+10j'), complex(10.4, 10));
asserts.isequal(str2double('-10.4+10j'), complex(-10.4, 10));
asserts.isequal(str2double('.4+10j'), complex(.4, 10));
asserts.isequal(str2double('+.4+10j'), complex(.4, 10));
asserts.isequal(str2double('-.4+10j'), complex(-.4, 10));
asserts.isequal(str2double('10+10.4j'), complex(10, 10.4));
asserts.isequal(str2double('10.4+10.4j'), complex(10.4, 10.4));
asserts.isequal(str2double('+10.4+10.4j'), complex(10.4, 10.4));
asserts.isequal(str2double('+10.4-10.4j'), complex(10.4, -10.4));
%=============================================================================
asserts.isequal(str2double('1i'), i);
asserts.isequal(str2double('1j'), i);
asserts.isequal(str2double('+1j'), i);
asserts.isequal(str2double('+1i'), i);
asserts.isequal(str2double('-1j'), complex(0, -1));
asserts.isequal(str2double('-1i'), complex(0, -1));
asserts.isequal(str2double('10j'), complex(0, 10));
asserts.isequal(str2double('+10j'), complex(0, 10));
asserts.isequal(str2double('-10j'), complex(0, -10));
asserts.isequal(str2double('10.j'), complex(0, 10));
asserts.isequal(str2double('+10.j'), complex(0, 10));
asserts.isequal(str2double('-10.j'), complex(0, -10));
asserts.isequal(str2double('.4j'), complex(0, .4));
asserts.isequal(str2double('+.4j'), complex(0, .4));
asserts.isequal(str2double('-.4j'), complex(0, -.4));
asserts.isequal(str2double('10.4j'), complex(0, 10.4));
asserts.isequal(str2double('+10.4j'), complex(0, 10.4));
asserts.isequal(str2double('-10.4j'), complex(0, -10.4));
%=============================================================================
asserts.isequal(str2double('3e2 + 2e3i'), complex(300, 2000));
asserts.isequal(str2double('3e2 - 2e3i'), complex(300, -2000));
asserts.isequal(str2double('3e2 - 2d3i'), NaN);
asserts.isequal(str2double('3e2 + 2d3i'), NaN);
asserts.isequal(str2double('3d2 + 2e3i'), NaN);
asserts.isequal(str2double('3d2 - 2e3i'), NaN);
asserts.isequal(str2double('3d2 + 2d3i'), NaN);
asserts.isequal(str2double('3d2 - 2d3i'), NaN);
%=============================================================================
asserts.isequal(str2double('1,2 + 3,4i'), complex(12, 34));
%=============================================================================
ce = {'2.718', '3.1416'; '137', '0.015'};
X = str2double(ce);
REF = [2.7180 3.1416; 137.0000 0.0150];
asserts.isapprox(X, REF, 1e-4);
%=============================================================================
X = str2double('1,,2');
REF = 12;
asserts.isequal(X, REF);
%=============================================================================
X = str2double('NaN*i');
REF = complex(0, NaN);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('Nan*i');
REF = complex(0, NaN);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('Inf*i');
REF = complex(0, Inf);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('Infi');
REF = complex(0, Inf);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('INFi');
REF = complex(0, Inf);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('-NaN*i');
REF = complex(0, NaN);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('-Nan*i');
REF = complex(0, NaN);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('-Inf*i');
REF = complex(0, -Inf);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('-Infi');
REF = complex(0, -Inf);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('-INFi');
REF = complex(0, -Inf);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('+NaN*i');
REF = complex(0, NaN);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('+Nan*i');
REF = complex(0, NaN);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('+Inf*i');
REF = complex(0, Inf);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('+Infi');
REF = complex(0, Inf);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('+INFi');
REF = complex(0, Inf);
asserts.isequal(X, REF);
%=============================================================================
X = str2double('InfI');
asserts.isequal(X, NaN);
%=============================================================================
ce = ["2.718", "3.1416"; "137", "0.015"];
X = str2double(ce);
REF = [2.7180 3.1416; 137.0000 0.0150];
asserts.isapprox(X, REF, 1e-4);
%=============================================================================
