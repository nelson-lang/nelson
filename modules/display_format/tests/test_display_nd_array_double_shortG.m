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
format('shortG')
%=============================================================================
A = rand(3,3,2);
A(1) = 0;
R = evalc('A');
REF = '
A(:,:,1) =

            0      0.91338       0.2785
      0.90579      0.63236      0.54688
      0.12699      0.09754      0.95751


A(:,:,2) =

      0.96489      0.95717      0.14189
      0.15761      0.48538      0.42176
      0.97059      0.80028      0.91574

';
assert_isequal(R, REF)
%=============================================================================
