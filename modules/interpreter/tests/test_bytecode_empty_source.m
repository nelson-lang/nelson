%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if iswasm()
  skip_testsuite('bytecode artifact engine is not available in the portable WebAssembly runtime.');
end
%=============================================================================
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
contents = {'', sprintf('%% Only a comment\n'), sprintf(' \n\t\n')};
for k = 1:numel(contents)
  source = fullfile(directory, ['empty_', num2str(k), '.m']);
  artifact = strrep(source, '.m', '.nbc');
  fid = fopen(source, 'wt');
  fprintf(fid, '%s', contents{k});
  fclose(fid);
  __bytecode_artifact__('compile', source, artifact);
  asserts.istrue(contains(__bytecode_artifact__('inspect', artifact), 'functions=1'));
  marker = 123;
  run(source);
  asserts.isequal(marker, 123);
  delete(source);
  run(artifact);
  asserts.isequal(marker, 123);
end
clear cleanup;
