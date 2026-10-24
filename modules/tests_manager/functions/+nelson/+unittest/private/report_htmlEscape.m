%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function txt = report_htmlEscape(value)
  if isempty(value)
    txt = '';
    return
  end
  txt = char(value);
  txt = replace(txt, '&', '&amp;');
  txt = replace(txt, '<', '&lt;');
  txt = replace(txt, '>', '&gt;');
  txt = replace(txt, '"', '&quot;');
  txt = replace(txt, char(39), '&#39;');
end
%=============================================================================
