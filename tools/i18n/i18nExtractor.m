%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function i18nExtractor(varargin)
  % Regenerates the four locale/*.json catalogs by scanning the modules/
  % sources natively (i18nHelpers extractUI/extractErrors builtins): no
  % gettext tooling is involved. With no input argument (build integration),
  % the interpreter exits when done.
  tic();

  modulesPath = [nelsonroot(), '/modules'];
  localePath = [nelsonroot(), '/locale/'];
  uiUS = [localePath, 'nelson-ui-en_US.json'];
  uiFR = [localePath, 'nelson-ui-fr_FR.json'];
  errorsUS = [localePath, 'nelson-errors-en_US.json'];
  errorsFR = [localePath, 'nelson-errors-fr_FR.json'];

  % User-interface catalog (keyed by the English source text): every _(),
  % _W() and TR() literal of the C/C++ sources and every _() literal of the
  % .m sources. Error/Warning templates are NOT harvested here: they belong
  % in the identifier-keyed errors catalog, never in the UI catalog.
  i18nHelpers('extractUI', modulesPath, uiUS);

  % Merge with the existing French UI catalog (preserves current translations,
  % prunes to the keys actually present in the UI catalog).
  i18nHelpers('merge', uiUS, uiFR);
  i18nHelpers('sort', uiUS, uiUS);
  i18nHelpers('sort', uiFR, uiFR);

  % Errors catalog (keyed by identifier): Error/ErrorAsCaller/Warning/
  % resolveMessageTemplate literal pairs in C/C++ and error/warning literal
  % pairs in .m. An identifier mapped to several distinct templates is
  % dropped (its sites keep their inline template) and reported.
  i18nHelpers('extractErrors', modulesPath, errorsUS);

  % Drop identifiers that are used somewhere with a run-time (non-literal)
  % cascade message (a macro / variable). Keeping a fixed template for them
  % would make the cascade override that run-time message. Detected
  % structurally from the sources, so there is no hand-maintained exclusion
  % list.
  i18nHelpers('noindex', errorsUS, modulesPath);

  % Build the French error catalog by carrying over any existing French error
  % translation, then filling remaining identifiers by looking their English
  % template up in the text-keyed French UI catalog (identical template =
  % same translation). Identifiers whose template changed keep an empty value
  % and fall back to English at runtime until re-translated.
  i18nHelpers('partition', errorsUS, errorsFR, uiFR);
  i18nHelpers('sort', errorsUS, errorsUS);
  i18nHelpers('sort', errorsFR, errorsFR);

  % Drop from the UI (text-keyed) catalogs the entries that only duplicate a
  % single-template error catalog entry: those are resolved through the
  % identifier cascade and never text-keyed, so the UI copy is dead. Entries
  % that are also used by a text-keyed helper are kept. Run AFTER the error
  % catalogs are built (the French error catalog is seeded from the UI French
  % catalog, which must still be complete at that point).
  i18nHelpers('dedup', uiUS, errorsUS, modulesPath);
  i18nHelpers('dedup', uiFR, errorsUS, modulesPath);

  toc();
  if nargin < 1
    exit('force');
  end
end
%=============================================================================
