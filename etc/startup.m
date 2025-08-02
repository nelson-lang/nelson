%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
true = 1; false = 0;
run([nelsonroot() '/modules/' 'modules.m']);
for module_state = modules_list'
  if (module_state{1}{2})
    run([nelsonroot(), '/modules/', module_state{1}{1}, '/loader.m']);
  end
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
