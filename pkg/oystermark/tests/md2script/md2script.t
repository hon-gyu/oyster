Encode a markdown file with frontmatter and python code blocks:

  $ cat > input.md << 'EOF'
  > ---
  > oyster:
  >   pyproject:
  >     version: "3.13"
  >     dependencies:
  >       - numpy
  > ---
  > # Title
  > 
  > Some text
  > 
  > ```python
  > import numpy as np
  > print(np.array([1, 2, 3]))
  > ```
  > 
  > More text
  > EOF

  $ md2script encode input.md
  # /// script
  # requires-python = ">=3.13"
  # dependencies = ["numpy"]
  # ///
  
  # %% [markdown]
  # # Title
  #
  # Some text
  
  # %%
  import numpy as np
  print(np.array([1, 2, 3]))
  
  # %% [markdown]
  #
  # More text




Encode without frontmatter:

  $ cat > no_fm.md << 'EOF'
  > # Hello
  > 
  > ```python
  > x = 1
  > ```
  > EOF

  $ md2script encode no_fm.md
  # %% [markdown]
  # # Hello
  
  # %%
  x = 1


Decode a percent-format script:

  $ cat > script.py << 'EOF'
  > # /// script
  > # requires-python = ">=3.13"
  > # dependencies = ["numpy"]
  > # ///
  > 
  > # %% [markdown]
  > # # Title
  > #
  > # Some text
  > 
  > # %%
  > import numpy as np
  > 
  > # %% [markdown]
  > # More text
  > EOF

  $ md2script decode script.py
  ---
  oyster:
    pyproject:
      version: "3.13"
      dependencies:
      - numpy
  ---
  # Title
  
  Some text
  ```python
  import numpy as np
  ```
  More text



Roundtrip: encode then decode preserves content:

  $ md2script encode input.md > roundtrip.py
  $ md2script decode roundtrip.py > roundtrip.md
  $ md2script encode roundtrip.md > roundtrip2.py
  $ diff roundtrip.py roundtrip2.py

Multiple consecutive code blocks:

  $ cat > multi.md << 'EOF'
  > # Analysis
  > 
  > ```python
  > import requests
  > ```
  > 
  > ```python
  > r = requests.get("https://example.com")
  > ```
  > 
  > Done.
  > EOF

  $ md2script encode multi.md
  # %% [markdown]
  # # Analysis
  
  # %%
  import requests
  
  # %%
  r = requests.get("https://example.com")
  
  # %% [markdown]
  #
  # Done.




Non-python code blocks stay in markdown cells:

  $ cat > mixed.md << 'EOF'
  > ```bash
  > echo hi
  > ```
  > 
  > ```python
  > print("hello")
  > ```
  > EOF

  $ md2script encode mixed.md
  # %% [markdown]
  # ```bash
  # echo hi
  # ```
  
  # %%
  print("hello")

