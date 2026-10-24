%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function frame = assert_opengl_patch_frame(f)
  hasContent = false;
  pauseState = pause('query');
  pause('on');
  figure(f);
  f.Position = [120 120 640 480];
  refresh(f);
  for k = 1:60
    drawnow();
    pause(0.05);
    drawnow();
    refresh(f);
    frame = __getframe__(f, true);
    if max(frame(:)) < 8
      skip_testsuite(true, 'OpenGL frame capture returned black frame.');
    end
    delta = max(frame, [], 3) - min(frame, [], 3);
    nonWhite = frame(:, :, 1) < 245 | frame(:, :, 2) < 245 | frame(:, :, 3) < 245;
    hasContent = nnz(nonWhite | delta > 8) > numel(nonWhite) * 0.001;
    if hasContent
      break
    end
  end
  pause(pauseState);
  asserts.istrue(ndims(frame) == 3);
  asserts.isequal(size(frame, 3), 3);
  asserts.istrue(hasContent);
end
