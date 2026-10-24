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
exampleRoot = fullfile(modulepath('graphics'), 'examples');
examplePath = fullfile(exampleRoot, 'abstract_particle_vortex.m');
asserts.istrue(isfile(examplePath));
asserts.isequal(parsefile(examplePath), 'script');

source = fileread(examplePath);
asserts.istrue(contains(source, 'targetFrameRate = 60;'));
asserts.istrue(contains(source, 'vortexInitialParticles'));
asserts.istrue(contains(source, 'vortexFrameParticles'));
asserts.istrue(contains(source, 'vortexParticleColors'));
asserts.istrue(contains(source, 'vortexDrawBackground'));
asserts.istrue(contains(source, 'vortexLightRays'));
asserts.istrue(contains(source, 'vortexParticleTails'));
asserts.istrue(contains(source, 'vortexStartAudio'));
asserts.istrue(contains(source, 'vortexAmbientAudio'));
asserts.istrue(contains(source, 'vortexStopAudio'));
asserts.istrue(contains(source, 'vortexRunAnimation'));
asserts.istrue(contains(source, '''CData'', particleColor'));
asserts.istrue(contains(source, '''SizeData'', markerSize'));
asserts.istrue(contains(source, '''s'', ''filled'''));

vortexFigureVisible = 'off';
vortexBatch = true;
vortexEnableAudio = false;
vortexParticleCount = 32;
vortexFrameCount = 7;
vortexMaxFrames = 4;
run(examplePath);

assert_isequal(targetFrameRate, 60);
assert_isequal(size(xHistory), [32 7]);
assert_isequal(size(yHistory), [32 7]);
assert_isequal(size(depthHistory), [32 7]);
renderedColumns = 1:vortexMaxFrames;
assert_istrue(all(isfinite(xHistory(:, renderedColumns)), 'all'));
assert_istrue(all(isfinite(yHistory(:, renderedColumns)), 'all'));
assert_istrue(all(isfinite(depthHistory(:, renderedColumns)), 'all'));
assert_istrue(min(depthHistory(:, renderedColumns), [], 'all') >= 0);
assert_istrue(max(depthHistory(:, renderedColumns), [], 'all') <= 1);
assert_istrue(isgraphics(figureHandle));
assert_istrue(isgraphics(axesHandle));
assert_istrue(isgraphics(particleHandle));
assert_istrue(isgraphics(glowHandle));
assert_istrue(isgraphics(highlightHandle));
assert_istrue(isgraphics(shadowHandle));
assert_istrue(isgraphics(tailHandle));
assert_istrue(isgraphics(rayHandle));
assert_istrue(isgraphics(coreHandle));
assert_isequal(numel(particleHandle.XData), vortexParticleCount);
assert_isequal(numel(particleHandle.YData), vortexParticleCount);
assert_isequal(numel(highlightHandle.XData), vortexParticleCount);
assert_isequal(numel(shadowHandle.XData), vortexParticleCount);
assert_isequal(numel(tailHandle.XData), 3 * vortexParticleCount);
assert_istrue(isempty(vortexAudioPlayer));
delete(figureHandle);
drawnow();

manifest = jsondecode(fileread(fullfile(exampleRoot, 'index.json')));
exampleFiles = strings(1, numel(manifest.examples));
for exampleIndex = 1:numel(manifest.examples)
  exampleFiles(exampleIndex) = manifest.examples(exampleIndex).file;
end
asserts.istrue(any(exampleFiles == "abstract_particle_vortex.m"));
%=============================================================================
