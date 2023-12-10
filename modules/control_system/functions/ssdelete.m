%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function model = ssdelete(varargin)
  % ssselect Extract subsystem from larger system.
  % sysOut = ssdelete(sysIn, INPUTS, OUTPUTS)  
  % sysOut = ssdelete(sysIn, INPUTS, OUTPUTS, STATES)
  narginchk(3, 4);
  nargoutchk(0, 1);
  
  sys = varargin{1};
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  
  inputs = varargin{2};
  outputs = varargin{3};
  
  if nargin == 3
    states = [];
  else 
    states = varargin{4};
  end
  
  A = sys.A;
  B = sys.B;
  C = sys.C;
  D = sys.D;
  [nx, na] = size(sys.A);
  [ny, nu] = size(sys.D);
  if (length(states) ~= nx)
    if ~isempty(sys.A)
      A(:, states) = [];
      A(states,:) = [];
    else
      A = [];
    end
    if ~isempty(sys.B),
      B(states, :) = [];
      B(:, inputs) = [];
    else 
      B =[];
    end
    if ~isempty(sys.C)
      C(:, states) = [];
      C(outputs, :) = [];
    else 
      C = [];
    end
  else
    A = [];
    B = [];
    C = [];
  end
  if (length(inputs) ~= nu) & (~isempty(sys.D))
    D(:, inputs) = [];
    D(outputs, :) = [];
  else
    D = [];
  end
  model = ss(A, B, C, D, sys.Ts);
end
