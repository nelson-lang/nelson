%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function txt = report_xmlEscape(txt)
  txt = replace(txt, '&', '&amp;');
  txt = replace(txt, '<', '&lt;');
  txt = replace(txt, '>', '&gt;');
  txt = replace(txt, '"', '&quot;');
end
