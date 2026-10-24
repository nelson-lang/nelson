%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function txt = report_oneLine(txt)
  txt = replace(txt, newline(), ' ');
  txt = replace(txt, char(10), ' ');
  txt = replace(txt, char(13), ' ');
end
