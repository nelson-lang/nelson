%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Grounding coverage harness: for a canonical request of each domain, the prompt
% aiComposePrompt assembles must carry the grounding that domain needs (verified
% API, per-domain inventory, the matching skill guide, the block catalog), end with
% the user's request, and stay under the size budget. This is the automated proxy
% for "does the model receive the right context" without driving the live agent.
%=============================================================================
privateDir = [modulepath('ai', 'root'), '/functions/+nelson/+ai/+internal/private'];
addpath(privateDir);
cleaner = onCleanup(@() rmpath(privateDir)); %#ok<NASGU>
haveWhat = (nargin('what') ~= 0);
%=============================================================================
% Every assembled prompt ends with the user's request, verbatim and last.
req = 'calcule la fft de mon signal';
p = aiComposePrompt(req, 'web', false);
assert_istrue(contains(p, 'User request:'));
assert_istrue(endsWith(p, req));
%=============================================================================
% Signal domain: the named function is grounded (verified API), and the domain
% inventory surfaces the signal modules.
p = aiComposePrompt('calcule la fft de mon signal et applique un filtre butter', 'web', false);
assert_istrue(contains(p, 'Verified Nelson API'));
if haveWhat
  assert_istrue(contains(p, 'Available Nelson functions by domain'));
  assert_istrue(contains(p, 'signal_processing:') || contains(p, 'fftw:'));
end
%=============================================================================
% Linear-algebra domain: the inventory surfaces the linear_algebra module.
if haveWhat
  p = aiComposePrompt('calcule les valeurs propres eig et la svd de ma matrice', 'web', false);
  assert_istrue(contains(p, 'linear_algebra:') || contains(p, 'Verified Nelson API'));
end
%=============================================================================
% NFlow acausal on an ACP surface off the pane: the physical block catalog and the
% host-action bridge are both present.
if ~isempty(nflowAcausalCatalog())
  p = aiComposePrompt('construis un circuit RLC acausal avec resistance', 'web', true);
  assert_istrue(contains(p, 'Physical (acausal)'));
  assert_istrue(contains(p, 'electrical:'));
end
%=============================================================================
% ACP history seeding: a FRESH ACP session is seeded with recent history (so a talk
% continued on the other surface, or after a restart, has context); an already-live
% ACP session is NOT re-seeded, and the direct backend always carries history.
seedSid = 'test_grounding_seed_zzz';
nelson.ai.internal.history.clear(seedSid);
cleanupSeed = onCleanup(@() nelson.ai.internal.history.clear(seedSid)); %#ok<NASGU>
nelson.ai.internal.history.append(seedSid, 'user', 'prior_seed_turn_zzz');
assert_istrue(contains(aiComposePrompt('q', seedSid, true, true), 'prior_seed_turn_zzz'));
assert_isequal(strfind(aiComposePrompt('q', seedSid, true, false), 'prior_seed_turn_zzz'), []);
assert_istrue(contains(aiComposePrompt('q', seedSid, false), 'prior_seed_turn_zzz'));
%=============================================================================
% Size budget: even a request that matches many domains at once stays under a hard
% context ceiling (the guardrail keeps the prompt from ballooning).
broad = ['filtre le signal fft, trace un plot, calcule svd et eig de la matrice, ', ...
  'construis un circuit RLC acausal avec resistance et condensateur, simule et ', ...
  'genere le code C du modele'];
p = aiComposePrompt(broad, 'web', true);
cut = strfind(p, 'User request:');
context = p(1:cut(1) - 1);
assert_istrue(length(context) < 20000);
%=============================================================================
function catalog = nflowAcausalCatalog()
  catalog = [];
  try
    catalog = NFlow.internal.acausalCatalog();
  catch
  end
end
%=============================================================================
