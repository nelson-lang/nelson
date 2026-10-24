%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = timetableHasEventtable(args)
  % An eventtable among the arguments makes the result an eventtable,
  % whatever its position.
  tf = false;
  for k = 1:numel(args)
    if isa(args{k}, 'eventtable')
      tf = true;
      return
    end
  end
end
%=============================================================================
