%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function filenames = extract(source, varargin)
  % Extracts an archive of any supported format into a folder (current folder by default).
  % Password, MaxExtractedSize and MaxCompressionRatio are name-value options.
  args = varargin;
  password = '';
  k = find(cellfun(@(v) (ischar(v) || isstring(v)) && strcmpi(char(v), 'Password'), args(1:end - 1)), 1);
  if ~isempty(k) && mod(numel(args) - k, 2) == 1
    password = args{k + 1};
    args(k:k + 1) = [];
  end
  a = nelson.io.Archive(source, 'Password', password);
  cleanup = onCleanup(@() a.close());
  filenames = a.extract(args{:});
end
%=============================================================================
