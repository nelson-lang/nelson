%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
contents = {'', sprintf('%% Only a comment\n'), sprintf(' \n\t\n')};
for k = 1:numel(contents)
  source = fullfile(directory, ['empty_', num2str(k), '.m']);
  fid = fopen(source, 'wt');
  fprintf(fid, '%s', contents{k});
  fclose(fid);
  plan = jsondecode(__nelsonc_plan__('link', source));
  asserts.istrue(plan.complete);
  asserts.isequal(plan.entryKind, 'script');
  asserts.isfalse(plan.dynamicFallback);
  asserts.isequal(numel(plan.files), 1);
end
clear cleanup;
