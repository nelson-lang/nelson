%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function uihtmlRecordDataChange(src, event)
  global UIHTML_DATA_CHANGES
  UIHTML_DATA_CHANGES{end + 1} = struct('data', event.Data, 'previous', event.PreviousData);
end
