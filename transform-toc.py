#!/usr/bin/env python3
"""
Transform pandoc-generated TOC to use HTML5 <details> elements for collapsible sections.
"""

import re
import sys
from pathlib import Path

try:
    from bs4 import BeautifulSoup
except ImportError:
    print("Error: beautifulsoup4 is required. Install with: pip3 install beautifulsoup4", file=sys.stderr)
    sys.exit(1)


def transform_list_to_details(ul_element, soup, level=0):
    """
    Recursively transform nested <ul> lists into <details> elements.
    """
    for li in ul_element.find_all('li', recursive=False):
        # Find the direct child ul (nested list)
        nested_ul = li.find('ul', recursive=False)

        if nested_ul:
            # This li has a nested list, wrap it in details
            # Get the link (first child that's an 'a')
            link = li.find('a', recursive=False)

            if link:
                # Create details and summary elements
                details = soup.new_tag('details')

                summary = soup.new_tag('summary')

                # Move the link into the summary
                link_copy = link.extract()
                summary.append(link_copy)

                # Add summary to details
                details.append(summary)

                # Recursively transform the nested ul
                transform_list_to_details(nested_ul, soup, level + 1)

                # Move the nested ul into details
                nested_ul_copy = nested_ul.extract()
                details.append(nested_ul_copy)

                # Clear the li and add the details
                li.clear()
                li.append(details)


def transform_toc(html_content):
    """
    Transform the TOC in the HTML content to use <details> elements.
    """
    soup = BeautifulSoup(html_content, 'html.parser')

    # Find the toc-content div
    toc_content = soup.find('div', class_='toc-content')

    if not toc_content:
        print("Warning: Could not find toc-content div", file=sys.stderr)
        return html_content

    # Find the first <ul> in toc-content (after search-container)
    search_container = toc_content.find('div', class_='search-container')
    if not search_container:
        print("Warning: Could not find search-container div", file=sys.stderr)
        return html_content

    # Get the ul that comes after search-container
    toc_ul = search_container.find_next_sibling('ul')
    if not toc_ul:
        print("Warning: Could not find TOC <ul> element", file=sys.stderr)
        return html_content

    # Transform the list
    transform_list_to_details(toc_ul, soup)

    # Wrap the transformed ul in a nav#TOC element
    toc_nav = soup.new_tag('nav', id='TOC')
    toc_ul_copy = toc_ul.extract()
    toc_nav.append(toc_ul_copy)

    # Insert the nav after search-container
    search_container.insert_after(toc_nav)

    return str(soup)


def main():
    if len(sys.argv) != 2:
        print("Usage: transform-toc.py <html-file>", file=sys.stderr)
        sys.exit(1)

    html_file = Path(sys.argv[1])

    if not html_file.exists():
        print(f"Error: File not found: {html_file}", file=sys.stderr)
        sys.exit(1)

    # Read the HTML file
    html_content = html_file.read_text(encoding='utf-8')

    # Transform the TOC
    transformed = transform_toc(html_content)

    # Write back to the same file
    html_file.write_text(transformed, encoding='utf-8')

    print(f"✓ Transformed TOC in {html_file}")


if __name__ == '__main__':
    main()
