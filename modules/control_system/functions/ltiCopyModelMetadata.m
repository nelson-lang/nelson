%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = ltiCopyModelMetadata(out, in)
  out.TimeUnit = in.TimeUnit;
  out.InputName = in.InputName;
  out.InputUnit = in.InputUnit;
  out.InputGroup = in.InputGroup;
  out.OutputName = in.OutputName;
  out.OutputUnit = in.OutputUnit;
  out.OutputGroup = in.OutputGroup;
  out.Notes = in.Notes;
  out.UserData = in.UserData;
  if ~isempty(in.Name)
    out.Name = in.Name;
  end
  out.SamplingGrid = in.SamplingGrid;
end
%=============================================================================
