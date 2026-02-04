# MSDS-498
Group Project for Northwestern MSDS 498

> ⚠️ **THIS GUIDE IS FOR WINDOWS ONLY**

---

# STEP 1: Install Git (Required)

## 1.1 Download and Install
1. Go to https://git-scm.com/downloads
2. Click **Windows**
3. The download should start automatically
4. Open the downloaded file (e.g., `Git-2.x.x-64-bit.exe`)
5. Click **Next** through the installer (default options are fine)
6. Click **Install**
7. Click **Finish**

## 1.2 Verify Installation
1. Press the Windows key and search for `cmd`
2. Open **Command Prompt** (it's a black box—not so scary, I promise)
3. Type this and press Enter:
```
   git --version
```
   If you see a version number, you're good!

## 1.3 Configure Git
Set up your name and email. These get attached to your commits.

> 💡 Honestly, the values here don't really matter—but use your real name and email if you want credit!
```
git config --global user.name "Your Name"
git config --global user.email "youremail@example.com"
```

---


# STEP 2: Setup collaborative folder on your computer

## Option A: Command Line

### A1: Clone the repo
1. Open Command Prompt
2. Run this command:
```
   git clone https://github.com/Andrew-Steingass/MSDS-498.git ~/Documents/MSDS-498
```
3. Check your Documents folder—you should see the repo

### A2: How to collaborate (do this every time)
1. Pull the latest changes:
```
   git pull
```
2. Add your changes:
```
   git add .
```
3. Commit with a message (change the message to describe what you did):
```
   git commit -m "your message here"
```
4. Push to the repo:
```
   git push
```

---

## Option B: GitHub Desktop (GUI app)
1. Go to https://desktop.github.com
2. Download and install it
3. Sign in with your GitHub account
4. Click **File → Clone Repository**
5. Find your repo or paste the URL
6. Choose where to save it (Documents)
7. Click **Clone**
