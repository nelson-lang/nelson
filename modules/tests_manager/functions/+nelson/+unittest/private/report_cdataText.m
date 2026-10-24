%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function txt = report_cdataText(txt)
  txt = regexprep(txt, '[\x00-\x08\x0B\x0C\x0E-\x1F]', '');
  txt = replace(txt, ']]>', ']]]]><![CDATA[>');
end
%=============================================================================
