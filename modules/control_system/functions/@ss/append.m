%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = append(varargin)
  for i = 1:length(varargin)
    if ~isa(varargin{i}, 'ss')
      sys = ss(varargin{i});
    else
      sys = varargin{i};
    end
    % Get the matrices
    A = sys.A;
    B = sys.B;
    C = sys.C;
    D = sys.D;
    
    % The first one sets the default delay and sample time
    if(i == 1)
      firstsampleTime = sys.Ts;
      Apast = blkdiag([], A);
      Bpast = blkdiag([], B);
      Cpast = blkdiag([], C);
      Dpast = blkdiag([], D);
    else
      sampleTime = sys.Ts;
      % sample time needs to be the same as the others
      if (firstsampleTime == sampleTime)
        Apast = blkdiag(Apast, A);
        Bpast = blkdiag(Bpast, B);
        Cpast = blkdiag(Cpast, C);
        Dpast = blkdiag(Dpast, D);
      else
        error(_('Sampling times must agree.'));
      end 
    end
  end
  sys = ss(Apast, Bpast, Cpast, Dpast);
  sys.Ts = firstsampleTime;
end
