%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Text of an ACP content holder (a session/update's params.update, or a result),
% tolerating a plain-string content and the {type:'text', text:...} form.
function out = acpContentText(holder)
  out = '';
  if ~isstruct(holder) || ~isfield(holder, 'content')
    return
  end
  content = holder.content;
  if ischar(content) || (isstring(content) && isscalar(content))
    out = char(content);
  elseif isstruct(content) && isscalar(content) && isfield(content, 'text')
    out = char(content.text);
  end
end
%=============================================================================
