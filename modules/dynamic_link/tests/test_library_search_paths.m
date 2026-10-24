%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
saved = __library_search_paths__();
restore = onCleanup(@() __library_search_paths__(saved));
__library_search_paths__({});
asserts.istrue(isempty(__library_search_paths__()));
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
if ispc()
  before = __library_search_paths__({directory, fullfile(directory, '.')});
  asserts.istrue(isempty(before));
  current = __library_search_paths__();
  asserts.isequal(numel(current), 1);
  asserts.istrue(isfolder(current{1}));
  assert_rejected(@() __library_search_paths__({directory, fullfile(directory, 'missing')}));
  asserts.isequal(__library_search_paths__(), current);
  assert_rejected(@() __library_search_paths__({'.'}));
  asserts.isequal(__library_search_paths__(), current);
  assert_rejected(@() __library_search_paths__(repmat({directory}, 1, 257)));
  asserts.isequal(__library_search_paths__(), current);
  previous = __library_search_paths__({});
  asserts.isequal(previous, current);
else
  assert_rejected(@() __library_search_paths__({directory}));
end
asserts.istrue(isempty(__library_search_paths__()));
clear cleanup;
clear restore;

function assert_rejected(callback)
  identifier = '';
  try
    callback();
  catch ex
    identifier = ex.identifier;
  end
  asserts.isequal(identifier, 'Nelson:nelsonc:nativeSearchPath');
end
