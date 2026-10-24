%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = test_run_save_results(test_suites, result_outputfile) %#ok<NLS0026>
  res = true;
  if ~isempty(result_outputfile) && ischar(result_outputfile)
    [p, f, e] = fileparts(result_outputfile); %#ok<NLS0004>
    if strcmp(e, '.*')
      res = save_as_xml(test_suites, [p, filesep, f, '.xml']);
      res = res && save_as_json(test_suites, [p, filesep, f, '.json']);
    elseif strcmp(e, '.xml')
      res = save_as_xml(test_suites, result_outputfile);
    elseif strcmp(e, '.json')
      res = save_as_json(test_suites, result_outputfile);
    else
      res = save_as_xml(test_suites, result_outputfile);
    end
  end
end
