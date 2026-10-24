%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
probePath = getenv('NELSON_QT_WORKSPACE_PROBE');
skip_testsuite(isempty(probePath) || ~isfile(probePath), 'Build the Qt workspace probe first.');
library = dlopen(probePath);
probe = dlsym(library, 'figureWorkspaceProbe', 'int32', {'cstring', 'cstring'});
for demo = {'stanford-bunny/stanford_bunny.m', 'utah-teapot/utah_teapot.m'}
  run([modulepath('graphics', 'root') '/examples/' demo{1}]);
  f = gcf(); ax = gca();
  titleText = 'Axis off mouse rotation';
  f.Name = titleText; f.NumberTitle = 'off';
  drawnow(); pause(0.1);
  before = ax.View;
  frame = jsondecode(__web_display_list__(f));
  geometry = __figure_native_geometry__(f);
  rect = frame.rotatableAxes(1).rect;
  point = [(rect(1) + 0.5 * rect(3)) * geometry(3), ...
    (1 - rect(2) - 0.5 * rect(4)) * geometry(4)];
  dlcall(probe, titleText, '{"operation":"key","key":"r"}');
  __figure_mouse_event__(f, 'press', point);
  __figure_mouse_event__(f, 'motion', point + [30 10]);
  __figure_mouse_event__(f, 'release', point + [30 10]);
  for index = 1:300
    drawnow(); pause(0.01);
    if index >= 3 && dlcall(probe, titleText, '{"operation":"idle"}') == 1
      break;
    end
  end
  asserts.istrue(norm(ax.View - before) > 1);
  asserts.isequal(char(ax.Visible), 'off');
  __web_figure_action__(__web_graphics_handle__(f, 'number'), '{"action":"undo"}', false);
  asserts.isapprox(ax.View, before, 1e-10);
  close(f);
end
dlclose(library);
