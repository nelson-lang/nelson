%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('loadnh5'), 1);
asserts.isequal(nargout('loadnh5'), 1);
%=============================================================================
H5_FILE = [fileparts(nfilename('fullpathext'), 'path'), '/h5save_schema_1.nh5'];
asserts.istrue(isfile(H5_FILE));
%=============================================================================
loadnh5(H5_FILE, 'A');
REF = logical(eye(5, 4));
asserts.isequal(A, REF);
%=============================================================================
R = loadnh5(H5_FILE, 'A');
REF = struct();
REF.A = logical(eye(5, 4));
asserts.isequal(R, REF);
%=============================================================================
clear('A', 'R');
%=============================================================================
loadnh5(H5_FILE);
%=============================================================================
% logical
A_REF = logical(eye(5, 4));
asserts.isequal(A, A_REF);
%=============================================================================
% double
B_REF = eye(5, 4);
asserts.isequal(B, B_REF);
%=============================================================================
% double complex
C_REF = eye(5, 4) + 2i;
asserts.isequal(C, C_REF);
%=============================================================================
% double empty
D_REF = ones(0, 3);
asserts.isequal(D, D_REF);
%=============================================================================
% single
E_REF = single(eye(5, 4));
asserts.isequal(E, E_REF);
%=============================================================================
% complex single
F_REF = single(eye(5, 4) + 2i);
asserts.isequal(F, F_REF);
%=============================================================================
% single empty
G_REF = single(ones(0, 3));
asserts.isequal(G, G_REF);
%=============================================================================
% unicode characters
H_REF = 'NelSon 象形字';
asserts.isequal(H, H_REF);
%=============================================================================
% unicode
I_REF = ['NelSon';
  'is    ';
  'not   ';
  'LensOn'];
asserts.isequal(I, I_REF);
%=============================================================================
% function_handle
asserts.isequal(func2str(J), 'sin');
%=============================================================================
asserts.isequal(func2str(JJ), '@(x)x+1');
%=============================================================================
% sparse double
K_REF = sparse(eye(3, 3));
asserts.isequal(K, K_REF);
%=============================================================================
% sparse double complex
L_REF = sparse(eye(3, 3) + 2i);
asserts.isequal(L, L_REF);
%=============================================================================
% sparse logical
M_REF = sparse(logical(eye(3, 3)));
asserts.isequal(M, M_REF);
%=============================================================================
% nd array double
N_REF = ones(3, 4, 2);
asserts.isequal(N, N_REF);
%=============================================================================
% nd array double complex
O_REF = complex(ones(3, 4, 2), 2);
asserts.isequal(O, O_REF);
%=============================================================================
% empty 2D cell
P_REF = cell(3, 4);
asserts.isequal(P, P_REF);
%=============================================================================
% cell
Q_REF = {'jim', 89, [5 2 1]; 'george', pi, 3i};
asserts.isequal(Q, Q_REF);
%=============================================================================
% nd array cell
R_REF = cell(3, 4, 2);
R_REF{1, 1} = 3;
R_REF{3, 4, 2} = 6;
asserts.isequal(R, R_REF);
%=============================================================================
% nd array logical
S_REF = ones(3, 4, 6);
S_REF = logical(S_REF);
asserts.isequal(S, S_REF);
%=============================================================================
% nd array int8
T_REF = ones(3, 4, 6);
T_REF = int8(T_REF);
asserts.isequal(T, T_REF);
%=============================================================================
% nd array uint8
U_REF = ones(3, 4, 6);
U_REF = uint8(U_REF);
asserts.isequal(U, U_REF);
%=============================================================================
% nd array int16
V_REF = ones(3, 4, 6);
V_REF = int16(V_REF);
asserts.isequal(V, V_REF);
%=============================================================================
% nd array uint16
W_REF = ones(3, 4, 6);
W_REF = uint16(W);
asserts.isequal(W, W_REF);
%=============================================================================
% nd array int32
X_REF = ones(3, 4, 6);
X_REF = int32(X_REF);
asserts.isequal(X, X_REF);
%=============================================================================
% nd array uint32
Y_REF = ones(3, 4, 6);
Y_REF = uint32(Y_REF);
asserts.isequal(Y, Y_REF);
%=============================================================================
% nd array int64
Z_REF = ones(3, 4, 6);
Z_REF = int64(Z_REF);
asserts.isequal(Z, Z_REF);
%=============================================================================
% nd array uint64
AA_REF = ones(3, 4, 6);
AA_REF = uint64(AA_REF);
asserts.isequal(AA, AA_REF);
%=============================================================================
% nd array single
AB_REF = ones(3, 4, 6);
AB_REF = single(AB_REF);
asserts.isequal(AB, AB_REF);
%=============================================================================
% nd array double
AC_REF = ones(3, 4, 6);
AC_REF = double(AC_REF);
asserts.isequal(AC, AC_REF);
%=============================================================================
% nd array char
AD_REF = ones(3, 4, 6);
AD_REF = char(AD_REF);
asserts.isequal(AD, AD_REF);
%=============================================================================
% uint8
AE_REF = ones(3, 4);
AE_REF = uint8(AE_REF);
asserts.isequal(AE, AE_REF);
%=============================================================================
% int8
AF_REF = ones(3, 4);
AF_REF = int8(AF_REF);
asserts.isequal(AF, AF_REF);
%=============================================================================
% uint16
AI_REF = ones(3, 4);
AI_REF = uint16(AI_REF);
asserts.isequal(AI, AI_REF);
%=============================================================================
% int16
AJ_REF = ones(3, 4);
AJ_REF = int16(AJ_REF);
asserts.isequal(AJ, AJ_REF);
%=============================================================================
% uint32
AK_REF = ones(3, 4);
AK_REF = uint32(AK_REF);
asserts.isequal(AK, AK_REF);
%=============================================================================
% int32
AL_REF = ones(3, 4);
AL_REF = int32(AL_REF);
asserts.isequal(AL, AL_REF);
%=============================================================================
% uint64
AM_REF = ones(3, 4);
AM_REF = uint64(AM_REF);
asserts.isequal(AM, AM_REF);
%=============================================================================
% int64
AN_REF = ones(3, 4);
AN_REF = int64(AN_REF);
asserts.isequal(AN, AN_REF);
%=============================================================================
AO_REF = struct('name', {'Pierre', 'Anna', 'Roberta'}, 'age', {43, 21, 31});
asserts.isequal(AO, AO_REF);
%=============================================================================
AP_REF = struct();
asserts.isequal(AP, AP_REF);
%=============================================================================
AQ_REF = struct([]);
asserts.isequal(AQ, AQ_REF);
%=============================================================================
AR_REF = struct(ones(1, 0));
asserts.isequal(AR, AR_REF);
%=============================================================================
AS_REF = struct(ones(0, 1));
asserts.isequal(AS, AS_REF);
%=============================================================================
AT_REF = struct(ones(3, 0, 2));
asserts.isequal(AT, AT_REF);
%=============================================================================
c = {'tree', 37.4, 'birch'};
f = {'category', 'height', 'name'};
AU_REF = cell2struct(c, f, 2);
asserts.isequal(AU, AU_REF);
%=============================================================================
c = {'birch', 'betula', 65; 'maple', 'acer', 50};
f = {'name', 'genus', 'height'};
AV_REF = cell2struct(c, f, 2);
asserts.isequal(AV, AV_REF);
%=============================================================================
my_cell_array = {'Jimmy', 'Timothy', 'Charles'};
AW_REF = cell2struct(cell(size(my_cell_array)), my_cell_array, 2);
asserts.isequal(AW, AW_REF);
%=============================================================================
clear C;
C{1} = 'tim';
C{2} = 'love';
C{3} = 1.73;
AX_REF = cell2struct(C, {'firstname', 'familyname', 'height'}, 2);
asserts.isequal(AX, AX_REF);
%=============================================================================
clear C;
C{1} = 'tim';
C{2} = 'love';
C{3} = 1.73;
AY_REF = cell2struct(C, {'firstname'});
asserts.isequal(AY, AY_REF);
%=============================================================================
AZ_REF = ["NelSon";
  "is";
  "not";
  "LensOn"];
asserts.isequal(AZ, AZ_REF);
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
cplx_REF = complexObj(3, 4);
asserts.isequal(class(cplx), class(cplx_REF));
asserts.isequal(cplx.r, cplx_REF.r);
asserts.isequal(cplx.i, cplx_REF.i);
%=============================================================================
