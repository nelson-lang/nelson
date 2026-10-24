%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_autoremove(varargin)
  options = parseOptions(varargin);
  orphans = nmm_orphans();
  report = struct();
  report.dry_run = options.dry_run;
  report.removed = orphans;
  report.changed = false;
  if ~options.dry_run
    for k = 1:length(orphans)
      nmm_uninstall(orphans(k).package, '-force');
    end
    report.changed = ~isempty(orphans);
  end
  report.ok = true;
  if options.json
    report = jsonprettyprint(jsonencode(report));
  end
end
%=============================================================================
function options = parseOptions(args)
  options = struct();
  options.dry_run = false;
  options.json = false;
  if length(args) > 2
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  for k = 1:length(args)
    value = args{k};
    if ~ischar(value)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', k + 1);
    end
    switch value
      case '-dry-run'
        options.dry_run = true;
      case '-json'
        options.json = true;
      otherwise
        nmm_error('InvalidOptionExpected', '%s expected.', sprintf('#%d argument: ''-dry-run'' or ''-json''', k + 1));
    end
  end
end
%=============================================================================
