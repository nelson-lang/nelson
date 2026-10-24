%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = newsfeed(url, varargin)
  narginchk(0, 2);
  nargoutchk(0, 2);
  if nargin < 1 || isempty(url)
    url = 'https://github.com/nelson-lang/nelson/releases.atom';
  end
  tohtml = any(strcmpi(varargin, '-tohtml'));
  tostruct = any(strcmpi(varargin, '-tostruct'));

  feed = newsfeedRead(url);

  if tostruct
    varargout{1} = feed;
    return;
  end

  html = newsfeedHtml(feed);

  if tohtml
    varargout{1} = html;
    return;
  end

  [status, h] = web(['text://', html], '-new', '-noaddressbox');
  if nargout >= 1; varargout{1} = h; end
  if nargout >= 2; varargout{2} = status; end
end
%=============================================================================
