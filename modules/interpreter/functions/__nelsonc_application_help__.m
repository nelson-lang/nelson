%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function shown = __nelsonc_application_help__(manifest, inputs)
  shown = false;
  if ~isfield(manifest, 'helpText')
    return;
  end
  text = manifest.helpText;
  if ~ischar(text) || (~isrow(text) && ~isempty(text)) ...
    || numel(text) > 1048576 || any(text == char(0))
    error('Nelson:nelsonc:manifest:invalidApplicationHelpText', 'Invalid application help text.');
  end
  shown = numel(inputs) == 1 && any(strcmp(inputs{1}, {'-?', '/?', '--help'}));
  if shown
    fprintf('%s', text);
  end
end
