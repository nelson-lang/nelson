%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
run([nelsonroot() '/modules/' 'modules.m']);
for k = modules_list'
  run([nelsonroot() '/modules/' k{1} '/loader.m']);
end
%==============================================================================
if (isquietmode() == false)
  banner();
  fprintf(stdout(), '\n%s\n', version());
  fprintf(stdout(), '%s\n', version('-date'));
  fprintf(stdout(), '%s\n', ' ');
end
clear('all');
cd(nelsonroot());
%==============================================================================
