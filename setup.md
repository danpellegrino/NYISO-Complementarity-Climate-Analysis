# Setting up R inside of WSL and VS Code

When I do development I like to be in a containerized environment and typically WSL gives me exactly what I want. If you want my exact development setup throughout this project, this is the page for how you can achieve this.

Please follow the instructions on how to install _WSL_ and the _Ubuntu_ distro from the Canonical page: https://canonical-ubuntu-wsl.readthedocs-hosted.com/en/latest/guides/install-ubuntu-wsl2/.

For reference, everything at the time of this project was done on `Ubuntu 22.04.3 LTS (Jammy Jellyfish)`.

Please follow the _Ubuntu_ instructions at The Comprehensive R Archive Network: https://cran.r-project.org/.

For reference, everything at the time of this project was done on `R 4.4.1 (Race for Your Life)`.

# Using Radian as your Rterm

Radian is a 21st century R console that looks a lot more modern than the traditional R terminal. You can find the project here: https://github.com/randy3k/radian.

Please make sure pip package installer is installed:

```bash
sudo apt install python3-pip
```

Install the `radian` package:
```python
pip install radian
```

Make sure your `~/.local/bin` is part of your `PATH` and update your `.bashrc`:
```bash
echo "PATH=\$PATH:~/.local/bin" >> ~/.bashrc
source ~/.bashrc
```


# Setting up VS Code

Please download VS Code: https://code.visualstudio.com/download.

## Dependencies and settings

Now in VS Code install the following extension:
- WSL: https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl

### Cloning and opening the repository

For the `languageserver` R package to work with Ubuntu you'll need the `libxml2-dev` package. Also, `git` for cloning this repository.

Open an Ubuntu terminal and do the following:
```bash
sudo apt install git libxml2-dev
```

Now clone and open:
```bash
git clone https://github.com/danpellegrino/NYISO-Complementarity-Climate-Analysis.git
cd NYISO-Complementarity-Climate-Analysis
code .
```

Now VS Code should install the WSL extension dependencies inside your Ubuntu container.

You'll want to install the following VS Code extension inside the WSL Container:
- R: https://marketplace.visualstudio.com/items?itemName=REditorSupport.r

In your VS Code settings you'll want to update the `Rterm: Linux` setting to the path your radian is at. To find out do the following in an Ubuntu terminal:
```bash
whereis radian
```

Then under the `Rterm: Linux` put the correct directory of its binary. It most likely follows this naming scheme: `/home/{USERNAME}/.local/bin/radian`.

For the formatting to be correct in the radian terminal as well make sure to enable the `Bracketed Paste` setting for the R extension.

Now reopen VS Code, and after it opens, you'll be requested to install the `languageserver` package say yes to this.

### Installing Project Dependencies

Some packages in this repository will require corresponding development libraries to compile. In an Ubuntu terminal:
```bash
sudo apt install libpng-dev libcurl4-openssl-dev
```

Bring up an R terminal inside VS Code and then type:
```R
renv::install()
```

And enter `Y` at the `Do you want to proceed?` prompt.

After this you're all set! 