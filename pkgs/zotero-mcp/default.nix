{
  lib,
  python3Packages,
}:

python3Packages.buildPythonApplication {
  pname = "zotero-mcp";
  version = "0.1.6";
  pyproject = true;

  src = python3Packages.fetchPypi {
    pname = "zotero_mcp";
    version = "0.1.6";
    hash = "sha256-yEeT/Lo9YuZ8rb8NN+blnx3kqqVg4lPRYH0HtLFq6zg=";
  };

  build-system = [
    python3Packages.hatchling
  ];

  dependencies = [
    python3Packages.mcp
    python3Packages.pydantic
    python3Packages.python-dotenv
    python3Packages.pyzotero
  ];

  meta = {
    description = "MCP server for Zotero reference management";
    homepage = "https://github.com/kujenga/zotero-mcp";
    license = lib.licenses.mit;
    mainProgram = "zotero-mcp";
    platforms = lib.platforms.linux;
  };
}
