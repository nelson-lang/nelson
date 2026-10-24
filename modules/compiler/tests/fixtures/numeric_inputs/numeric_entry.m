%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function numeric_entry(varargin)
  if ~isdeployed() || ismodule('compiler'); error('Invalid deployed environment.'); end
  raw = argv('user');
  if nargin() == 0; disp('NO_INPUTS_OK'); return; end
  if ischar(varargin{1})
    if ~isequal(raw(:), varargin(:)); error('Character arguments changed.'); end
    disp('CHAR_INPUTS_OK');
    return;
  end
  expected = [3.25, -250, 1 + 2i, NaN, NaN, Inf, -Inf, NaN, NaN, NaN, ...
    NaN, NaN, NaN, NaN, NaN, 17, 1200, -3i];
  if numel(varargin) ~= numel(expected); error('Incorrect numeric argument count.'); end
  for k = 1:numel(expected)
    value = varargin{k};
    if ~isa(value, 'double') || ~isscalar(value) || ~isequaln(value, expected(k))
      error('Unexpected numeric argument %d: %s.', k, jsonencode(value));
    end
    if ~ischar(raw{k}); error('argv must retain the original text.'); end
  end
  if ~isempty(raw{4}) || ~strcmp(raw{15}, '--help') || ~strcmp(raw{16}, '  17  ')
    error('Original command-line text changed.');
  end
  disp('NUMERIC_INPUTS_OK');
end
