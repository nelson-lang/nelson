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
    if (i == 1)
      sysPast = sys;
      firstsampleTime = sys.Ts;
      Apast = blkdiag([], A);
      Bpast = blkdiag([], B);
      Cpast = blkdiag([], C);
      Dpast = blkdiag([], D);
    else
      firstsampleTime = ltiResolveSampleTime(sysPast, sys);
      [pPast, mPast] = size(Dpast);
      [pSys, mSys] = size(D);
      sysPast.InputName = ltiMergeCellMetadata(sysPast.InputName, mPast, sys.InputName, mSys, 'concat');
      sysPast.InputUnit = ltiMergeCellMetadata(sysPast.InputUnit, mPast, sys.InputUnit, mSys, 'concat');
      sysPast.OutputName = ltiMergeCellMetadata(sysPast.OutputName, pPast, sys.OutputName, pSys, 'concat');
      sysPast.OutputUnit = ltiMergeCellMetadata(sysPast.OutputUnit, pPast, sys.OutputUnit, pSys, 'concat');
      sysPast.TimeUnit = ltiMergeTimeUnit(sysPast, sys);
      sysPast.UserData = ltiMergeUserData(sysPast, sys);
      sysPast.Ts = firstsampleTime;
      Apast = blkdiag(Apast, A);
      Bpast = blkdiag(Bpast, B);
      Cpast = blkdiag(Cpast, C);
      Dpast = blkdiag(Dpast, D);
    end
  end
  sys = ss(Apast, Bpast, Cpast, Dpast);
  sys.Ts = firstsampleTime;
  sys.TimeUnit = sysPast.TimeUnit;
  sys.InputName = sysPast.InputName;
  sys.InputUnit = sysPast.InputUnit;
  sys.OutputName = sysPast.OutputName;
  sys.OutputUnit = sysPast.OutputUnit;
  if ~isempty(sysPast.UserData)
    sys.UserData = sysPast.UserData;
  end
end
