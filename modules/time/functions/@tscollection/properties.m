%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = properties(obj)
  names = [{'Name'; 'Time'; 'TimeInfo'; 'Length'}; fieldnames(obj.Members_)];
end
%=============================================================================
