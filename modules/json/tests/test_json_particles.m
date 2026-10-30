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
jsonParticlesFigureVisible = 'off';
jsonParticlesAnimationEnabled = true;
jsonParticlesAnimationFrames = 40;
jsonParticlesFramePause = 0;
jsonParticlesAnimationContinuous = false;
run([modulepath('json', 'root'), '/examples/json_particles.m']);

assert_istrue(isgraphics(jsonParticlesFigure));
% The scene was decoded from JSON into a struct with three emitters.
assert_isequal(jsonParticlesEmitterCount, 3);
assert_isequal(numel(jsonParticlesScatter), 3);
assert_istrue(all(isgraphics(jsonParticlesScatter)));
% Particles are alive and confined to the unit square after many frames.
totalParticles = 0;
for emitterIndex = 1:jsonParticlesEmitterCount
  particles = jsonParticlesState{emitterIndex};
  totalParticles = totalParticles + size(particles, 1);
  if ~isempty(particles)
    assert_istrue(all(particles(:, 2) >= 0));
  end
end
assert_istrue(totalParticles > 0);
delete(jsonParticlesFigure);
drawnow();
%=============================================================================
