# 📌 Connecting Nelson with Ollama: A Basic Guide

## 🚀 Step 1: Set Up Ollama Server

First, visit [Ollama's website](https://ollama.com/) and follow the download instructions.

To use local models with Ollama, install and start an Ollama server. Then, pull models into the server. For example, to pull `llama3`, open your terminal and run:

```bash
ollama pull llama3
```

## 📂 Step 2: Set Up Nelson

Ensure the `webtools/examples/ollama` module is accessible by adding its path in Nelson:

```matlab
addpath([modulepath('webtools'), '/examples/ollama'])
```

## 🤖 Step 3: Query Ollama from Nelson

You can now interact with Ollama directly from Nelson. For example, to get a short answer to "How to calculate covariance?", use:

```matlab
ollamaPrompt('Get short answer to ''How to calculate covariance ?''')
```

Or, to get a more detailed response:

```matlab
ollamaPrompt('How to calculate covariance ?')
```

---

🔹 _This guide provides a simple way to integrate Nelson with Ollama for AI-powered queries!_ 🚀
