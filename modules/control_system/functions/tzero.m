%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = tzero(varargin)
  % Generates the MIMO zeros from a transfer function or state space model
  % z = tzero(sys)
  % z = tzero(A, B, C, D, E)
  % [z, nrank] = tzero(sys)
  % [z, nrank] = tzero(A, B, C, D, E)
  
  nargoutchk(0, 2);
  narginchk(1, 5);
  if islti(varargin{1})
    narginchk(1, 1);
    sys = ss(varargin{1});
  else
    narginchk(4, 5);
    A = varargin{1};
    B = varargin{2};
    C = varargin{3};
    D = varargin{4};
    sys = ss(A, B, C, D);
    if nargin == 5
      E = varargin{5};
      if ~isequal(size(A), size(E))
        error(_('A and E must have the same size.'));
      end
      sys.E = E;
    end
    if nargout == 2
      [z, nrank] = tzero(sys);
      varargout{1} = z;
      varargout{2} = nrank;
    else
      z = tzero(sys);
      varargout{1} = z;
    end
    return
  end
  
  % Get necessary info
  A = sys.A;
  sizeOrginalA = size(A, 2);
  B = sys.B;
  C = sys.C;
  D = sys.D;
  % Increase A at maximum size
  column = [size(B, 2) size(C, 2) size(D, 2)];
  row = [size(B, 1) size(C, 1) size(D, 1)];
  A = [A zeros(size(A, 1), max(column) - size(A, 2))];
  A = [A; zeros(max(row) - size(A, 1), size(A, 2))];
  % Make sure so matrix B, C, D have the same size as A
  C = rechange(A, C);
  B = rechange(A, B);
  D = rechange(A, D);
  % Compute the zeros
  M = [A, B; C, D];
  I = [eye(size(A, 1), size(A, 2)) B*0; C*0, D*0];
  z = eig(M, I);
  z = z(isfinite(z)); % Remove the inf and zero values from z
  % check the lengt of z so we can remove some 0 0 0 from z
  if(length(z) - sizeOrginalA > 0)
    % Ok! We have more zeros that orginal size of A
    % Remove all 0 0 0
    z = z(z ~= 0);
    % Then add new one!
    % if sizeOrginalA - length(z) <= 0, then it will be no change
    z = [z, zeros(1, sizeOrginalA - length(z))]; % add 0 if needed
  end
  varargout{1} = z;
  if nargout == 2
    p = pole(varargin{1});
    dc = dcgain(varargin{1});
    varargout{2} = dc*prod(-p)/prod(-z);
  end
end
%=============================================================================
% Make sure so matrix B, C, D have the same size as A
function [matrix] = rechange(A, mat)
  if(size(A, 1) - size(mat, 1) >= 0)
    matrix = [mat; zeros(size(A, 1) - size(mat, 1), size(mat,2))];
  end
  if(size(A, 2) - size(matrix, 2) >= 0)
    matrix = [matrix zeros(size(matrix,1), size(A, 2) - size(matrix, 2))];
  end
end
%=============================================================================
