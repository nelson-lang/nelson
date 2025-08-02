%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [r, url, tag_branch] = nmm_is_http_repository(param)
  r = false;
  if ((startsWith(param, 'http://') == 1 || startsWith(param, 'https://') == 1) && ~isempty(strfind(param, '.git')))
    tag_index = strfind(param, '#');
    if isempty(tag_index)
      r = true;
      url = param;
      tag_branch = '';
    else
      if isscalar(tag_index)
        r = true;
        url = param(1:tag_index - 1);
        tag_branch = param(tag_index + 1 : end);
      else
        r = false;
        url = '';
        tag_branch = '';
      end
    end
  end
end
%=============================================================================
