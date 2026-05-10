import { execFile } from "node:child_process"
import { promisify } from "node:util"

const execFileAsync = promisify(execFile)
const cacheTtlMs = 30_000

const envCache = new Map()

const home = process.env.HOME
const miseBin = process.env.MISE_BIN ?? (home ? `${home}/.local/bin/mise` : "mise")

async function loadMiseEnv(cwd) {
  const now = Date.now()
  const cached = envCache.get(cwd)
  if (cached && now - cached.at < cacheTtlMs) {
    return cached.env
  }

  const { stdout } = await execFileAsync(miseBin, ["hook-env", "-s", "nu", "--force"], {
    cwd,
    env: process.env,
  })

  const env = {}
  for (const line of stdout.split(/\r?\n/)) {
    if (!line) continue

    const [action, name, ...value] = line.split(",")
    if (action !== "set" || !name) continue

    env[name] = value.join(",")
  }

  envCache.set(cwd, { env, at: now })
  return env
}

export const MiseEnvPlugin = async ({ client }) => {
  return {
    "shell.env": async (input, output) => {
      try {
        Object.assign(output.env, await loadMiseEnv(input.cwd))
      } catch (error) {
        await client.app.log({
          body: {
            service: "mise-env-plugin",
            level: "warn",
            message: `Failed to load mise env: ${error.message}`,
          },
        })
      }
    },
  }
}
