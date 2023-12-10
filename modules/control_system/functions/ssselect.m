%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function model = ssselect(varargin)
  % ssselect Extract subsystem from larger system.
  % sysOut = ssselect(sysIn, INPUTS, OUTPUTS)  
  % sysOut = ssselect(sysIn, INPUTS, OUTPUTS, STATES)
  narginchk(3, 4);
  nargoutchk(0, 1);
  
  sys = varargin{1};
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  
  inputs = varargin{2};
  outputs = varargin{3};
  
  [nx, na] = size(sys.A);
  
  if nargin == 3
    states = 1:nx;
  else 
    states = varargin{4};
  end
  
  A = [];
  B = [];
  C = [];
  D = [];
  
  if ~isempty(sys.A)
    A = sys.A(states, states);
  end
  if ~isempty(sys.B)
    B = sys.B(states,inputs);
  end
  if ~isempty(sys.C)
    C = sys.C(outputs,states);
  end
  if ~isempty(sys.D)
    D = sys.D(outputs,inputs);
  end
  model = ss(A, B, C, D, sys.Ts);  
end
