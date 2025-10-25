%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = need_to_be_generated(md_filename, html_filename)
  tf = true;
  html_file_info = dir(html_filename);
  md_file_info = dir(md_filename);
  if isempty(html_file_info.datenum) || isempty(md_file_info.datenum)
    return
  end
  tf = md_file_info.datenum > html_file_info.datenum;
end
%=============================================================================