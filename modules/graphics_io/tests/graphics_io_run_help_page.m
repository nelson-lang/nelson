%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function graphics_io_run_help_page(language, page, expectedCount)
  filename = [modulepath('graphics_io'), '/help/', language, '/xml/', page, '.xml'];
  asserts.istrue(xmldocchecker(filename));
  examples = graphics_io_cli_examples(filename);
  for exampleIndex = 1:length(examples)
    graphics_io_run_help_example(examples{exampleIndex}, filename, exampleIndex);
  end
  asserts.isequal(length(examples), expectedCount);
end
%=============================================================================
