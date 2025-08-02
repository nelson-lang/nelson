%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('loadnh5'), 1);
assert_isequal(nargout('loadnh5'), 1);
%=============================================================================
H5_FILE = [fileparts(nfilename('fullpathext'), 'path'), '/h5save_schema_1.nh5'];
assert_istrue(isfile(H5_FILE));
%=============================================================================
loadnh5(H5_FILE, 'A');
REF = logical(eye(5, 4));
assert_isequal(A, REF);
%=============================================================================
R = loadnh5(H5_FILE, 'A');
REF = struct();
REF.A = logical(eye(5, 4));
assert_isequal(R, REF);
%=============================================================================
clear('A', 'R');
%=============================================================================
loadnh5(H5_FILE);
%=============================================================================
% logical
A_REF = logical(eye(5, 4));
assert_isequal(A, A_REF);
%=============================================================================
% double
B_REF = eye(5, 4);
assert_isequal(B, B_REF);
%=============================================================================
% double complex
C_REF = eye(5,4) + 2i;
assert_isequal(C, C_REF);
%=============================================================================
% double empty
D_REF = ones(0,3);
assert_isequal(D, D_REF);
%=============================================================================
% single
E_REF = single(eye(5,4));
assert_isequal(E, E_REF);
%=============================================================================
% complex single
F_REF = single(eye(5,4) + 2i);
assert_isequal(F, F_REF);
%=============================================================================
% single empty
G_REF = single(ones(0,3));
assert_isequal(G, G_REF);
%=============================================================================
% unicode characters
H_REF = 'NelSon 象形字';
assert_isequal(H, H_REF);
%=============================================================================
% unicode
I_REF = ['NelSon';
'is    ';
'not   ';
'LensOn'];
assert_isequal(I, I_REF);
%=============================================================================
% function_handle
assert_isequal(func2str(J), 'sin');
%=============================================================================
assert_isequal(func2str(JJ), '@(x) x+1');
%=============================================================================
% sparse double
K_REF = sparse(eye(3,3));
assert_isequal(K, K_REF);
%=============================================================================
% sparse double complex
L_REF = sparse(eye(3,3) + 2i );
assert_isequal(L, L_REF);
%=============================================================================
% sparse logical
M_REF = sparse(logical(eye(3,3)));
assert_isequal(M, M_REF);
%=============================================================================
% nd array double
N_REF = ones(3,4,2);
assert_isequal(N, N_REF);
%=============================================================================
% nd array double complex
O_REF = complex(ones(3,4,2), 2);
assert_isequal(O, O_REF);
%=============================================================================
% empty 2D cell
P_REF = cell(3,4);
assert_isequal(P, P_REF);
%=============================================================================
% cell
Q_REF = {'jim', 89, [5 2 1] ; 'george', pi, 3i};
assert_isequal(Q, Q_REF);
%=============================================================================
% nd array cell
R_REF = cell(3, 4, 2);
R_REF{1, 1} = 3;
R_REF{3, 4, 2} = 6;
assert_isequal(R, R_REF);
%=============================================================================
% nd array logical
S_REF = ones(3, 4, 6);
S_REF = logical(S_REF);
assert_isequal(S, S_REF);
%=============================================================================
% nd array int8
T_REF = ones(3, 4, 6);
T_REF = int8(T_REF);
assert_isequal(T, T_REF);
%=============================================================================
% nd array uint8
U_REF = ones(3, 4, 6);
U_REF = uint8(U_REF);
assert_isequal(U, U_REF);
%=============================================================================
% nd array int16
V_REF = ones(3, 4, 6);
V_REF = int16(V_REF);
assert_isequal(V, V_REF);
%=============================================================================
% nd array uint16
W_REF = ones(3, 4, 6);
W_REF = uint16(W);
assert_isequal(W, W_REF);
%=============================================================================
% nd array int32
X_REF = ones(3,4,6);
X_REF = int32(X_REF);
assert_isequal(X, X_REF);
%=============================================================================
% nd array uint32
Y_REF = ones(3, 4, 6);
Y_REF = uint32(Y_REF);
assert_isequal(Y, Y_REF);
%=============================================================================
% nd array int64
Z_REF = ones(3, 4, 6);
Z_REF = int64(Z_REF);
assert_isequal(Z, Z_REF);
%=============================================================================
% nd array uint64
AA_REF = ones(3, 4, 6);
AA_REF = uint64(AA_REF);
assert_isequal(AA, AA_REF);
%=============================================================================
% nd array single
AB_REF = ones(3, 4, 6);
AB_REF = single(AB_REF);
assert_isequal(AB, AB_REF);
%=============================================================================
% nd array double
AC_REF = ones(3, 4, 6);
AC_REF = double(AC_REF);
assert_isequal(AC, AC_REF);
%=============================================================================
% nd array char
AD_REF = ones(3, 4, 6);
AD_REF = char(AD_REF);
assert_isequal(AD, AD_REF);
%=============================================================================
% uint8
AE_REF = ones(3, 4);
AE_REF = uint8(AE_REF);
assert_isequal(AE, AE_REF);
%=============================================================================
% int8
AF_REF = ones(3, 4);
AF_REF = int8(AF_REF);
assert_isequal(AF, AF_REF);
%=============================================================================
% uint16
AI_REF = ones(3, 4);
AI_REF = uint16(AI_REF);
assert_isequal(AI, AI_REF);
%=============================================================================
% int16
AJ_REF = ones(3, 4);
AJ_REF = int16(AJ_REF);
assert_isequal(AJ, AJ_REF);
%=============================================================================
% uint32
AK_REF = ones(3, 4);
AK_REF = uint32(AK_REF);
assert_isequal(AK, AK_REF);
%=============================================================================
% int32
AL_REF = ones(3, 4);
AL_REF = int32(AL_REF);
assert_isequal(AL, AL_REF);
%=============================================================================
% uint64
AM_REF = ones(3, 4);
AM_REF = uint64(AM_REF);
assert_isequal(AM, AM_REF);
%=============================================================================
% int64
AN_REF = ones(3, 4);
AN_REF = int64(AN_REF);
assert_isequal(AN, AN_REF);
%=============================================================================
AO_REF = struct('name', {'Pierre', 'Anna', 'Roberta'}, 'age', {43, 21, 31});
assert_isequal(AO, AO_REF);
%=============================================================================
AP_REF = struct();
assert_isequal(AP, AP_REF);
%=============================================================================
AQ_REF = struct([]);
assert_isequal(AQ, AQ_REF);
%=============================================================================
AR_REF = struct(ones(1,0));
assert_isequal(AR, AR_REF);
%=============================================================================
AS_REF = struct(ones(0,1));
assert_isequal(AS, AS_REF);
%=============================================================================
AT_REF = struct(ones(3, 0, 2));
assert_isequal(AT, AT_REF);
%=============================================================================
c = {'tree', 37.4, 'birch'};
f = {'category','height','name'};
AU_REF = cell2struct(c, f, 2);
assert_isequal(AU, AU_REF);
%=============================================================================
c = {'birch','betula',65;  'maple','acer',50};
f = {'name', 'genus', 'height'};
AV_REF = cell2struct(c, f, 2);
assert_isequal(AV, AV_REF);
%=============================================================================
my_cell_array = {'Jimmy', 'Timothy', 'Charles'};
AW_REF = cell2struct(cell(size(my_cell_array)), my_cell_array, 2);
assert_isequal(AW, AW_REF);
%=============================================================================
clear C;
C{1}='tim';
C{2}='love';
C{3}=1.73;
AX_REF = cell2struct(C,{'firstname','familyname','height'},2);
assert_isequal(AX, AX_REF);
%=============================================================================
clear C;
C{1}='tim';
C{2}='love';
C{3}=1.73;
AY_REF = cell2struct(C,{'firstname'});
assert_isequal(AY, AY_REF);
%=============================================================================
AZ_REF = ["NelSon";
"is";
"not";
"LensOn"];
assert_isequal(AZ, AZ_REF);
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
cplx_REF = complexObj(3, 4);
assert_isequal(class(cplx), class(cplx_REF));
assert_isequal(cplx.r, cplx_REF.r);
assert_isequal(cplx.i, cplx_REF.i);
%=============================================================================
