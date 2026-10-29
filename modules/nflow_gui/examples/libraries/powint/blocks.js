// =============================================================================
// Example library: Integer Power (powint) - renderers only
// =============================================================================
// Geometry (width, height, ports, defaultParams) lives in library.json.
// Simulation and code generation are handled by the native C++ plugin
// (powint.cpp). This file only exports the SVG render function.
// =============================================================================

/**
 * blockDefinitions - type -> BlockDefinition
 */
export const blockDefinitions = {
  powint: {
    render(block, { svgRect, createSvgElement }) {
      const g = block.group;
      g.appendChild(svgRect(0, 0, block.width, block.height, 'nf-block'));

      // u with a superscript n, drawn as two texts.
      g.appendChild(
        createSvgElement(
          'text',
          {
            x: block.width / 2 - 6,
            y: block.height / 2 + 4,
            class: 'block-text upright',
            'text-anchor': 'middle',
            'dominant-baseline': 'middle',
            'font-size': '18',
          },
          'u'
        )
      );
      g.appendChild(
        createSvgElement(
          'text',
          {
            x: block.width / 2 + 6,
            y: block.height / 2 - 6,
            class: 'block-text upright',
            'text-anchor': 'middle',
            'dominant-baseline': 'middle',
            'font-size': '11',
          },
          'n'
        )
      );
    },
  },
};

// Backward-compatible alias for existing libraries/tests
export const renderers = {
  powint: (block, helpers) => blockDefinitions.powint.render(block, helpers),
};
