%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlPreBlock(title, value)
  if isempty(value)
    lines = cell(0, 1);
    return
  end
  lines = {['<section><h2>', report_htmlEscape(title), '</h2><pre>', ...
    report_htmlEscape(value), '</pre></section>']};
  lines = lines(:);
end
%=============================================================================
