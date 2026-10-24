%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TT = mergevars(TT, varargin)
  % The table operation runs on the variables; the row times and the
  % timetable properties are kept.
  T = mergevars(tableObject(TT), varargin{:});
  TT = timetable.fromTableAndTimes(T, TT.Properties.RowTimes, TT.Properties);
end
%=============================================================================
