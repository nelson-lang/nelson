%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = length(obj)
  error('Nelson:table:UndefinedLengthFunction', sprintf('Undefined function ''length'' for input arguments of type ''%s''. Use the height, width, or size functions instead.', class(obj)));
end
%=============================================================================
