%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isunicodesupported()
  % This can be useful to decide whether to use Unicode characters or fallback ASCII characters in command-line output.
  % Note that the check is quite naive BUT it works.
  if strcmp(getenv('NELSON_TERM_IS_UNICODE_SUPPORTED'), 'TRUE')
    tf = true;
    return
  end
  if contains(getnelsonmode(), {'GUI', 'ADVANCED_SIO_CLIENT', 'BASIC_SIO_CLIENT'})
    tf = true;
    return
  end
  
  tf = false;
  if ~ispc()
    tf = ~strcmp(getenv('TERM'), 'linux');
    return
  end
  tf = ~isempty(getenv('WT_SESSION')) || ...
  strcmp(getenv('ConEmuTask'),  '{cmd::Cmder}') || ...
  strcmp(getenv('TERM_PROGRAM'), 'vscode') || ...
  strcmp(getenv('TERM'), 'xterm-256color') || ...
  strcmp(getenv('TERM'), 'alacritty');
end
%=============================================================================
