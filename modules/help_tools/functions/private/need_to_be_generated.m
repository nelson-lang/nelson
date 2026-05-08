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
  if isempty(html_file_info.datenum)
    return
  end
  if isdir(md_filename)
    files = dir([md_filename, '/*.xml'], '-s');
    if isempty(files)
      return
    end
    latest = files(1).datenum;
    for k = 2:length(files)
      if files(k).datenum > latest
        latest = files(k).datenum;
      end
    end
    tf = latest > html_file_info.datenum;
  else
    md_file_info = dir(md_filename);
    if isempty(md_file_info.datenum)
      return
    end
    tf = md_file_info.datenum > html_file_info.datenum;
  end
end
%=============================================================================
