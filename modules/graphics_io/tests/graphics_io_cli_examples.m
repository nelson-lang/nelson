function examples = graphics_io_cli_examples(filename)
  %=============================================================================
  % Copyright (c) 2016-present Allan CORNET (Nelson)
  %=============================================================================
  % This file is part of Nelson.
  %=============================================================================
  % LICENCE_BLOCK_BEGIN
  % SPDX-License-Identifier: LGPL-3.0-or-later
  % LICENCE_BLOCK_END
  %=============================================================================
  text = fileread(filename);
  pattern = '<example_item_data[^>]*runnable="adv-cli"[^>]*><!\[CDATA\[([\s\S]*?)\]\]></example_item_data>';
  tokens = regexp(text, pattern, 'tokens');
  examples = cell(1, length(tokens));
  for k = 1:length(tokens)
    examples{k} = tokens{k}{1};
  end
  %=============================================================================
