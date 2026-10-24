// =============================================================================
// Example library: Cumulative Product (cumprod) - renderers only
// =============================================================================
// Geometry (width, height, ports, defaultParams) lives in library.json.
// The library descriptor is derived from the manifest by the library-loader.
// Simulation is handled exclusively by the native C++ plugin (cumprod.cpp).
//
// This file only needs to export `renderers`: a map of block type → render
// function.  The render function receives (block, helpers) where helpers
// are the standard SVG helper utilities.
// =============================================================================

/**
 * blockDefinitions - type -> BlockDefinition
 * New API: return BlockDefinition objects; keep `renderers` alias for
 * backward compatibility.
 */
export const blockDefinitions = {
  cumprod: {
    render(block, { svgRect, createSvgElement }) {
      const g = block.group;
      g.appendChild(svgRect(0, 0, block.width, block.height, 'nf-block'));

      // ∏  (U+220F PRODUCT sign)
      g.appendChild(
        createSvgElement(
          'text',
          {
            x: block.width / 2,
            y: block.height / 2 - 4,
            class: 'block-text upright',
            'text-anchor': 'middle',
            'dominant-baseline': 'middle',
            'font-size': '18',
          },
          '\u220F'
        )
      );

      g.appendChild(
        createSvgElement(
          'text',
          {
            x: block.width / 2,
            y: block.height / 2 + 14,
            class: 'block-text upright',
            'text-anchor': 'middle',
            'dominant-baseline': 'middle',
            'font-size': '9',
          },
          'cum'
        )
      );
    },
  },
};

// Backward-compatible alias for existing libraries/tests
export const renderers = {
  cumprod: (block, helpers) => blockDefinitions.cumprod.render(block, helpers),
};
