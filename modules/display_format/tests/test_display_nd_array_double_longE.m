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
rng('default')
format('longE')
%=============================================================================
A = rand(3,3,2);
A(1) = 0;
R = evalc('A');
REF = '
A(:,:,1) =

                         0     9.133758556563407e-01     2.784982183948159e-01
     9.057919341139495e-01     6.323592499829829e-01     5.468815190251917e-01
     1.269868118688464e-01     9.754040162079036e-02     9.575068296398968e-01


A(:,:,2) =

     9.648885338101536e-01     9.571669497527182e-01     1.418863451108336e-01
     1.576130765024573e-01     4.853756483644247e-01     4.217612857464701e-01
     9.705927788745612e-01     8.002804731950164e-01     9.157355236820877e-01

';
assert_isequal(R, REF)
%=============================================================================
