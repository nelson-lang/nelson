%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% parse a temporary SKILL.md front-matter
root = [tempdir(), 'nelson_skill_test_', createGUID()];
mkdir(root);
skillDir = [root, '/mytool'];
mkdir(skillDir);
skillFile = [skillDir, '/SKILL.md'];
fid = fopen(skillFile, 'wt');
fprintf(fid, '---\nname: my-skill\ndescription: A test skill\ntriggers: plot, graph, figure\n---\n\n# Body\n');
fclose(fid);
meta = nelson.ai.parseSkill(skillFile);
assert_isequal(meta.name, 'my-skill');
assert_isequal(meta.description, 'A test skill');
assert_isequal(numel(meta.triggers), 3);
assert_istrue(any(strcmp(meta.triggers, 'graph')));
%=============================================================================
% a SKILL.md with no name falls back to its folder name
noNameFile = [skillDir, '/empty.md'];
fid = fopen(noNameFile, 'wt');
fprintf(fid, '# no front matter here\n');
fclose(fid);
metaNoName = nelson.ai.parseSkill([skillDir, '/SKILL.md']);
assert_isequal(metaNoName.name, 'my-skill');
rmdir(root, 's');
%=============================================================================
% the built-in catalog is discovered, each entry has a name and a real file
skills = nelson.ai.skills();
assert_istrue(~isempty(skills));
for k = 1:numel(skills)
  assert_istrue(~isempty(skills(k).name));
  assert_istrue(isfile(skills(k).path));
  assert_isequal(skills(k).source, 'builtin');
end
%=============================================================================
% trigger filtering returns a subset
allCount = numel(nelson.ai.skills());
subset = nelson.ai.skills('please make a plot of my data');
assert_istrue(numel(subset) <= allCount);
%=============================================================================
