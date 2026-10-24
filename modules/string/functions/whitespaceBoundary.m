%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pat = whitespaceBoundary(boundary)
  narginchk(0, 1);
  nargoutchk(0, 1);
  if nargin == 0 || strcmpi(char(boundary), 'either')
    expr = '(?:(?<=\s)(?=\S)|(?<=\S)(?=\s)|(?<=^)(?=\s)|(?<=\s)(?=$))';
  elseif strcmpi(char(boundary), 'start')
    expr = '(?<!\s)(?=\s)';
  elseif strcmpi(char(boundary), 'end')
    expr = '(?<=\s)(?!\s)';
  else
    error('Nelson:string:boundaryStartEndEither', 'Boundary must be ''start'', ''end'', or ''either''.');
  end
  pat = pattern(expr);
end
%=============================================================================
