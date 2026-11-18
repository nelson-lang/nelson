%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Lever B domain context: the per-domain function inventory (what(<module>)), the
% off-pane NFlow physical block catalog, and the live NFlow model snapshot. These
% read live from the build (what / acausalCatalog / an open model), so each block
% is guarded on its prerequisite being present.
%=============================================================================
privateDir = [modulepath('ai', 'root'), '/functions/+nelson/+ai/+internal/private'];
addpath(privateDir);
cleaner = onCleanup(@() rmpath(privateDir)); %#ok<NASGU>
%=============================================================================
% Per-domain function inventory: a signal request surfaces real signal_processing
% names, with a clear header, and only clean public names (no class methods).
if nargin('what') ~= 0
  inv = aiModuleInventoryContext('filtre mon signal et calcule la fft', 'web');
  assert_istrue(~isempty(inv));
  assert_istrue(contains(inv, 'Available Nelson functions by domain'));
  assert_istrue(contains(inv, 'signal_processing:'));
  assert_istrue(contains(inv, 'butter') || contains(inv, 'freqz'));
  % the inventory lists only plain names, never class methods or qualified names
  assert_isequal(strfind(inv, '@'), []);
  % a request matching no end-user skill yields no inventory
  assert_istrue(isempty(aiModuleInventoryContext('zzz qwerty foobar', 'web')));
end
%=============================================================================
% Off-pane NFlow physical block catalog: a web/Qt acausal request lists physical
% component types with their pins; the NFlow pane and non-NFlow requests get none.
if ~isempty(nflowAcausalCatalog())
  % a physical request names the acausal electrical components with their pins, and
  % a domain is honored (electrical cue -> no rotational/thermal noise)
  cat = aiNflowBlockCatalog('construis un circuit RLC acausal avec resistance', 'web');
  assert_istrue(~isempty(cat));
  assert_istrue(contains(cat, 'acausal'));
  assert_istrue(contains(cat, 'electrical:'));
  assert_istrue(contains(cat, 'Resistor'));
  assert_istrue(contains(cat, '['));
  assert_isequal(strfind(cat, 'thermal:'), []);
  % a pure signal request gets causal blocks (name-ranked, French term mapped) and
  % NO physical components
  sig = aiNflowBlockCatalog('un modele avec un gain une saturation et un integrateur', 'web');
  assert_istrue(~isempty(sig));
  assert_istrue(contains(sig, 'Signal (causal)'));
  assert_istrue(contains(sig, 'gain') && contains(sig, 'integrator'));
  assert_isequal(strfind(sig, 'Physical (acausal)'), []);
  % never on the NFlow pane (it injects its own live catalog)
  assert_istrue(isempty(aiNflowBlockCatalog('circuit rlc acausal', 'nflow')));
  % never for a request that is not about NFlow
  assert_istrue(isempty(aiNflowBlockCatalog('trace un plot simple', 'web')));
end
%=============================================================================
% Live NFlow model snapshot: an open model is reported with its blocks and their
% types; once closed, no NFlow line remains.
if exist('new_system') == 5 && exist('gcs') == 5
  new_system('test_domain_ctx_model');
  add_block('nflow/source/constant', 'test_domain_ctx_model/C', 'Value', 1);
  add_block('nflow/sink/scope', 'test_domain_ctx_model/S');
  add_line('test_domain_ctx_model', 'C/1', 'S/1');
  s = aiLiveSessionContext('nflow');
  assert_istrue(contains(s, 'NFlow model open'));
  assert_istrue(contains(s, 'test_domain_ctx_model'));
  assert_istrue(contains(s, 'C (constant)'));
  assert_istrue(contains(s, 'S (scope)'));
  bdclose('all');
  s2 = aiLiveSessionContext('web');
  assert_isequal(strfind(s2, 'NFlow model open'), []);
end
%=============================================================================
function catalog = nflowAcausalCatalog()
  catalog = [];
  try
    catalog = NFlow.internal.acausalCatalog();
  catch
  end
end
%=============================================================================
