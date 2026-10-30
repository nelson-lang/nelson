%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
xmlSceneFigureVisible = 'off';
xmlSceneAnimationEnabled = true;
xmlSceneAnimationFrames = 10;
xmlSceneFramePause = 0;
xmlSceneAnimationContinuous = false;
xmlSceneBallCount = 10;
run([modulepath('xml', 'root'), '/examples/xml_scene.m']);

assert_istrue(isgraphics(xmlSceneFigure));
% The scene was written to and read back from an XML file.
assert_istrue(isfile(xmlSceneFile));
assert_isequal(numel(xmlSceneBalls), xmlSceneBallCount);
assert_isequal(numel(xmlSceneMarkers), xmlSceneBallCount);
% Every ball stays inside the unit box it bounces in.
assert_istrue(all(xmlScenePositionX >= 0 & xmlScenePositionX <= 1));
assert_istrue(all(xmlScenePositionY >= 0 & xmlScenePositionY <= 1));
delete(xmlSceneFigure);
drawnow();
%=============================================================================
